module Config = struct
  type t =
    { port : int
    ; upstreams : string array
    ; workers : int
    ; buf_size : int
    }

  let int_env name default =
    match Sys.getenv_opt name with
    | Some value -> (try int_of_string value with _ -> default)
    | None -> default
  ;;

  let upstreams () =
    let value =
      match Sys.getenv_opt "UPSTREAMS" with
      | Some value -> value
      | None -> "/sockets/api1.sock,/sockets/api2.sock"
    in
    value
    |> String.split_on_char ','
    |> List.filter_map (fun s ->
      let s = String.trim s in
      if String.length s = 0 then None else Some s)
    |> Array.of_list
  ;;

  let load () =
    let upstreams = upstreams () in
    if Array.length upstreams = 0 then failwith "UPSTREAMS is empty";
    { port = int_env "PORT" 9999
    ; upstreams
    ; workers = max 1 (int_env "LB_WORKERS" 32)
    ; buf_size = max 4096 (int_env "BUF_SIZE" 8192)
    }
  ;;
end

module Socket = struct
  let close_quietly fd =
    try Unix.close fd with
    | _ -> ()
  ;;

  let listen_tcp port =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt fd Unix.SO_REUSEADDR true;
    Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen fd 4096;
    fd
  ;;

  let configure_client fd =
    try Unix.setsockopt fd Unix.TCP_NODELAY true with
    | _ -> ()
  ;;

  let connect_upstream path =
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd (Unix.ADDR_UNIX path);
    fd
  ;;

  let[@inline always] write_all fd b len =
    let rec loop pos =
      if pos < len
      then (
        let n = Unix.write fd b pos (len - pos) in
        if n = 0 then raise End_of_file;
        loop (pos + n))
    in
    loop 0
  ;;
end

module Http = struct
  let[@zero_alloc] [@inline always] ascii_lower_code c =
    let code = Char.code c in
    if code >= 65 && code <= 90 then code + 32 else code
  ;;

  let[@zero_alloc] [@inline always] is_header_ws c = c = ' ' || c = '\t'

  let[@zero_alloc] find_header_end b len =
    let rec loop i =
      if i + 3 >= len
      then -1
      else if Bytes.unsafe_get b i = '\r'
              && Bytes.unsafe_get b (i + 1) = '\n'
              && Bytes.unsafe_get b (i + 2) = '\r'
              && Bytes.unsafe_get b (i + 3) = '\n'
      then i + 4
      else loop (i + 1)
    in
    loop 0
  ;;

  let[@zero_alloc] content_length_header_at b start stop =
    stop - start >= 15
    &&
    let rec loop i =
      if i = 15
      then true
      else (
        let expected =
          match i with
          | 0 -> 99
          | 1 -> 111
          | 2 -> 110
          | 3 -> 116
          | 4 -> 101
          | 5 -> 110
          | 6 -> 116
          | 7 -> 45
          | 8 -> 108
          | 9 -> 101
          | 10 -> 110
          | 11 -> 103
          | 12 -> 116
          | 13 -> 104
          | _ -> 58
        in
        ascii_lower_code (Bytes.unsafe_get b (start + i)) = expected && loop (i + 1))
    in
    loop 0
  ;;

  let[@zero_alloc] skip_header_ws b i stop =
    let rec loop i =
      if i < stop && is_header_ws (Bytes.unsafe_get b i) then loop (i + 1) else i
    in
    loop i
  ;;

  let[@zero_alloc] parse_header_int b i stop =
    let rec loop i acc =
      if i < stop
      then (
        let c = Bytes.unsafe_get b i in
        if c >= '0' && c <= '9'
        then loop (i + 1) ((acc * 10) + Char.code c - 48)
        else acc)
      else acc
    in
    loop (skip_header_ws b i stop) 0
  ;;

  let[@zero_alloc] line_stop b i stop =
    let rec loop j =
      if j >= stop
      then stop
      else if Bytes.unsafe_get b j = '\r'
              && j + 1 < stop
              && Bytes.unsafe_get b (j + 1) = '\n'
      then j
      else loop (j + 1)
    in
    loop i
  ;;

  let[@zero_alloc] rec content_length_loop b header_end i content_length =
    if i >= header_end - 2
    then content_length
    else (
      let line_stop = line_stop b i header_end in
      if line_stop <= i
      then content_length
      else if content_length_header_at b i line_stop
      then
        content_length_loop
          b
          header_end
          (line_stop + 2)
          (parse_header_int b (i + 15) line_stop)
      else content_length_loop b header_end (line_stop + 2) content_length)
  ;;

  let[@zero_alloc] content_length b header_end =
    let first_line_stop = line_stop b 0 header_end in
    if first_line_stop = header_end
    then 0
    else content_length_loop b header_end (first_line_stop + 2) 0
  ;;

  let read_message fd buf =
    let cap = Bytes.length buf in
    let rec read_headers len =
      let header_end = find_header_end buf len in
      if header_end >= 0
      then read_body len header_end
      else if len = cap
      then raise End_of_file
      else (
        let n = Unix.read fd buf len (cap - len) in
        if n = 0 then raise End_of_file;
        read_headers (len + n))
    and read_body len header_end =
      let body_len = content_length buf header_end in
      let total = header_end + body_len in
      if total > cap
      then raise End_of_file
      else if len >= total
      then total
      else (
        let n = Unix.read fd buf len (total - len) in
        if n = 0 then raise End_of_file;
        read_body (len + n) header_end)
    in
    read_headers 0
  ;;
end

module Proxy = struct
  type worker =
    { upstreams : string array
    ; next_upstream : int ref
    ; request_buf : bytes
    ; response_buf : bytes
    }

  let create_worker upstreams buf_size =
    { upstreams
    ; next_upstream = ref (Unix.getpid () mod Array.length upstreams)
    ; request_buf = Bytes.create buf_size
    ; response_buf = Bytes.create buf_size
    }
  ;;

  let[@inline always] choose_upstream worker =
    let upstreams = worker.upstreams in
    let i = !(worker.next_upstream) in
    worker.next_upstream := (i + 1) mod Array.length upstreams;
    upstreams.(i)
  ;;

  let rec connect_with_retry worker attempts =
    try Socket.connect_upstream (choose_upstream worker) with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.ECONNREFUSED), _, _)
      when attempts > 0 -> connect_with_retry worker (attempts - 1)
  ;;

  let forward_response upstream client response_buf =
    let response_len = Http.read_message upstream response_buf in
    Socket.write_all client response_buf response_len
  ;;

  let handle_client worker client =
    try
      Socket.configure_client client;
      let request_len = Http.read_message client worker.request_buf in
      let upstream = connect_with_retry worker 64 in
      (try
         Socket.write_all upstream worker.request_buf request_len;
         forward_response upstream client worker.response_buf;
         Socket.close_quietly upstream;
         Socket.close_quietly client
       with
       | exn ->
         Socket.close_quietly upstream;
         Socket.close_quietly client;
         raise exn)
    with
    | _ -> Socket.close_quietly client
  ;;
end

module Process = struct
  let would_block = function
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> true
    | _ -> false
  ;;

  let rec reap_children () =
    match Unix.waitpid [ Unix.WNOHANG ] (-1) with
    | 0, _ -> ()
    | _ -> reap_children ()
    | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
  ;;

  let fork_workers count f =
    let rec loop remaining =
      if remaining <= 1
      then ()
      else (
        match Unix.fork () with
        | 0 -> ()
        | _pid -> loop (remaining - 1))
    in
    loop count;
    f ()
  ;;
end

let worker_loop server upstreams buf_size =
  let worker = Proxy.create_worker upstreams buf_size in
  while true do
    try
      let client, _ = Unix.accept server in
      Proxy.handle_client worker client
    with
    | exn when Process.would_block exn -> ()
    | Unix.Unix_error _ -> ()
  done
;;

let main () =
  Sys.Safe.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.Safe.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> Process.reap_children ()));
  let config = Config.load () in
  let server = Socket.listen_tcp config.port in
  Process.fork_workers config.workers (fun () ->
    worker_loop server config.upstreams config.buf_size)
;;

let () = main ()
