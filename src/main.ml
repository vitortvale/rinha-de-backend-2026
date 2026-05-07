module Std_unix = Unix

open Core
open Async
open Rinha_lib

type request =
  { route : route
  ; content_length : int
  ; close : bool
  }

and route =
  | Ready
  | Fraud_score
  | Other

type connection_state =
  { query : int array
  ; scorer : Index.scorer
  ; mutable body_bytes : Bytes.t
  ; mutable body_string : string
  }

let response status reason ?(content_type = "text/plain") ?(connection = "keep-alive") body =
  sprintf
    "HTTP/1.1 %d %s\r\nContent-Length: %d\r\nContent-Type: %s\r\nConnection: \
     %s\r\n\r\n%s"
    status
    reason
    (String.length body)
    content_type
    connection
    body

let response_json score =
  let approved = Float.(score < 0.6) in
  sprintf {|{"approved":%s,"fraud_score":%.1f}|} (Bool.to_string approved) score

let static_response ?connection status reason ?(content_type = "text/plain") body =
  response status reason ~content_type ?connection body

let ready_response = static_response 200 "OK" "OK"
let ready_response_close = static_response ~connection:"close" 200 "OK" "OK"

let bad_request_response =
  static_response ~connection:"close" 400 "Bad Request" "bad request"

let not_found_response = static_response 404 "Not Found" "not found"
let not_found_response_close =
  static_response ~connection:"close" 404 "Not Found" "not found"

let fraud_responses =
  Array.init (Index.k + 1) ~f:(fun frauds ->
    let score = Float.of_int frauds /. Float.of_int Index.k in
    static_response 200 "OK" ~content_type:"application/json" (response_json score))

let fraud_responses_close =
  Array.init (Index.k + 1) ~f:(fun frauds ->
    let score = Float.of_int frauds /. Float.of_int Index.k in
    static_response
      200
      "OK"
      ~connection:"close"
      ~content_type:"application/json"
      (response_json score))

let create_connection_state index =
  { query = Array.create ~len:Vectorize.dim 0
  ; scorer = Index.create_scorer index
  ; body_bytes = Bytes.create 0
  ; body_string = ""
  }

let ensure_body_buffer state len =
  if Bytes.length state.body_bytes < len
  then (
    let body_bytes = Bytes.create (Int.ceil_pow2 len) in
    state.body_bytes <- body_bytes;
    state.body_string <- Stdlib.Bytes.unsafe_to_string body_bytes)

let ensure_warmed =
  let warmed = ref false in
  fun index ->
    if not !warmed
    then (
      Index.prewarm index;
      warmed := true)

let starts_with line prefix =
  let line_len = String.length line in
  let prefix_len = String.length prefix in
  line_len >= prefix_len
  &&
  let rec loop i =
    i = prefix_len || (Char.equal line.[i] prefix.[i] && loop (i + 1))
  in
  loop 0

let parse_request_line line =
  if starts_with line "POST /fraud-score " then Fraud_score
  else if starts_with line "GET /ready " then Ready
  else Other

let ascii_lower_code c =
  let code = Char.to_int c in
  if code >= Char.to_int 'A' && code <= Char.to_int 'Z' then code + 32 else code

let caseless_equal_at line prefix =
  let line_len = String.length line in
  let prefix_len = String.length prefix in
  line_len >= prefix_len
  &&
  let rec loop i =
    i = prefix_len || (ascii_lower_code line.[i] = Char.to_int prefix.[i] && loop (i + 1))
  in
  loop 0

let skip_header_ws line i =
  let len = String.length line in
  let rec loop i =
    if i < len && (Char.equal line.[i] ' ' || Char.equal line.[i] '\t')
    then loop (i + 1)
    else i
  in
  loop i

let parse_header_int line i =
  let len = String.length line in
  let rec loop i acc =
    if i < len
    then (
      let c = line.[i] in
      if Char.(c >= '0' && c <= '9')
      then loop (i + 1) ((acc * 10) + Char.to_int c - Char.to_int '0')
      else acc)
    else acc
  in
  loop (skip_header_ws line i) 0

let value_is_close line i =
  let i = skip_header_ws line i in
  i + 5 <= String.length line
  && ascii_lower_code line.[i] = Char.to_int 'c'
  && ascii_lower_code line.[i + 1] = Char.to_int 'l'
  && ascii_lower_code line.[i + 2] = Char.to_int 'o'
  && ascii_lower_code line.[i + 3] = Char.to_int 's'
  && ascii_lower_code line.[i + 4] = Char.to_int 'e'

let parse_header line =
  if caseless_equal_at line "content-length:"
  then `Content_length (parse_header_int line 15)
  else if caseless_equal_at line "connection:"
  then `Connection_close (value_is_close line 11)
  else `Other

let read_headers reader =
  Reader.read_line reader
  >>= function
  | `Eof -> return None
  | `Ok request_line ->
    let route = parse_request_line request_line in
    let rec loop content_length close =
      Reader.read_line reader
      >>= function
      | `Eof -> return None
      | `Ok "" -> return (Some { route; content_length; close })
      | `Ok line ->
        (match parse_header line with
         | `Content_length content_length -> loop content_length close
         | `Connection_close close_requested -> loop content_length (close || close_requested)
         | `Other -> loop content_length close)
    in
    loop 0 false

let read_body state reader len =
  if len = 0
  then return (Some "")
  else (
    ensure_body_buffer state len;
    (if len < Bytes.length state.body_bytes then Bytes.unsafe_set state.body_bytes len '\000');
    Reader.really_read reader ~len state.body_bytes
    >>| function
    | `Ok -> Some state.body_string
    | `Eof _ -> None)

let write_and_flush writer payload =
  Writer.write writer payload;
  Writer.flushed writer

let write_and_close writer payload =
  write_and_flush writer payload
  >>= fun () -> Writer.close writer

let set_socket_options socket =
  try
    let fd = Socket.fd socket |> Fd.file_descr_exn in
    Core_unix.setsockopt fd Core_unix.TCP_NODELAY true;
    match Linux_ext.settcpopt_bool with
    | Error _ -> ()
    | Ok set_tcpopt_bool -> set_tcpopt_bool fd Linux_ext.TCP_QUICKACK true
  with
  | _ -> ()

let tcp_reuseport_socket () =
  try
    let socket = Socket.create Socket.Type.tcp in
    Socket.setopt socket Socket.Opt.reuseaddr true;
    Socket.setopt socket Socket.Opt.reuseport true;
    Some socket
  with
  | _ -> None

let rec handle_connection config index state reader writer =
  read_headers reader
  >>= function
  | None -> Writer.close writer
  | Some request ->
    read_body state reader request.content_length
    >>= (function
     | None -> write_and_close writer bad_request_response
     | Some body ->
       (match request.route with
        | Ready ->
          ensure_warmed index;
          let response = if request.close then ready_response_close else ready_response in
          write_response_or_continue config index state reader writer request response
        | Fraud_score ->
          (try
            ensure_warmed index;
            Vectorize.to_quantized_into config body state.query;
            let frauds = Index.score_frauds_with_scorer index state.scorer state.query in
            let response =
              if request.close
              then fraud_responses_close.(frauds)
              else fraud_responses.(frauds)
            in
            write_response_or_continue config index state reader writer request response
          with
          | _ -> write_and_close writer bad_request_response)
        | Other ->
          let response =
            if request.close then not_found_response_close else not_found_response
          in
          write_response_or_continue config index state reader writer request response))

and write_response_or_continue config index state reader writer request payload =
  write_and_flush writer payload
  >>= fun () ->
  if request.close then Writer.close writer else handle_connection config index state reader writer

type epoll_client =
  { fd : Std_unix.file_descr
  ; state : connection_state
  ; mutable input : Bytes.t
  ; mutable input_len : int
  ; mutable output : string
  ; mutable output_pos : int
  ; mutable close_after_output : bool
  }

let fd_to_int fd = Core_unix.File_descr.to_int fd

let strip_trailing_cr line =
  let len = String.length line in
  if len > 0 && Char.equal line.[len - 1] '\r'
  then String.sub line ~pos:0 ~len:(len - 1)
  else line

let parse_headers_block block =
  match String.split block ~on:'\n' with
  | [] -> None
  | request_line :: headers ->
    let route = parse_request_line (strip_trailing_cr request_line) in
    let rec loop content_length close = function
      | [] -> Some { route; content_length; close }
      | line :: rest ->
        let line = strip_trailing_cr line in
        (match parse_header line with
         | `Content_length content_length -> loop content_length close rest
         | `Connection_close close_requested -> loop content_length (close || close_requested) rest
         | `Other -> loop content_length close rest)
    in
    loop 0 false headers

let find_headers_end bytes len =
  let rec loop i =
    if i + 3 >= len
    then None
    else if Char.equal (Bytes.unsafe_get bytes i) '\r'
            && Char.equal (Bytes.unsafe_get bytes (i + 1)) '\n'
            && Char.equal (Bytes.unsafe_get bytes (i + 2)) '\r'
            && Char.equal (Bytes.unsafe_get bytes (i + 3)) '\n'
    then Some i
    else loop (i + 1)
  in
  loop 0

let ensure_input_capacity client =
  if client.input_len = Bytes.length client.input
  then (
    let next = Bytes.create (Bytes.length client.input * 2) in
    Bytes.blit ~src:client.input ~src_pos:0 ~dst:next ~dst_pos:0 ~len:client.input_len;
    client.input <- next)

let compact_input client consumed =
  let remaining = client.input_len - consumed in
  if remaining > 0
  then
    Bytes.blit ~src:client.input ~src_pos:consumed ~dst:client.input ~dst_pos:0 ~len:remaining;
  client.input_len <- remaining

let queue_epoll_response client request payload =
  client.output <- payload;
  client.output_pos <- 0;
  client.close_after_output <- request.close

let queue_epoll_close_response client payload =
  client.output <- payload;
  client.output_pos <- 0;
  client.close_after_output <- true

let score_epoll_request config index client request body =
  match request.route with
  | Ready ->
    ensure_warmed index;
    let payload = if request.close then ready_response_close else ready_response in
    queue_epoll_response client request payload
  | Fraud_score ->
    (try
      ensure_warmed index;
      Vectorize.to_quantized_into config body client.state.query;
      let frauds =
        Index.score_frauds_with_scorer index client.state.scorer client.state.query
      in
      let payload =
        if request.close then fraud_responses_close.(frauds) else fraud_responses.(frauds)
      in
      queue_epoll_response client request payload
    with
    | _ -> queue_epoll_close_response client bad_request_response)
  | Other ->
    let payload =
      if request.close then not_found_response_close else not_found_response
    in
    queue_epoll_response client request payload

let process_epoll_input config index client =
  let rec loop () =
    if String.is_empty client.output && not client.close_after_output
    then (
      match find_headers_end client.input client.input_len with
      | None -> ()
      | Some header_end ->
        let header_bytes = header_end + 4 in
        let header = Stdlib.Bytes.sub_string client.input 0 header_end in
        (match parse_headers_block header with
         | None -> queue_epoll_close_response client bad_request_response
         | Some request ->
           let total = header_bytes + request.content_length in
           if client.input_len >= total
           then (
             ensure_body_buffer client.state request.content_length;
             if request.content_length > 0
             then (
               Bytes.blit
                 ~src:client.input
                 ~src_pos:header_bytes
                 ~dst:client.state.body_bytes
                 ~dst_pos:0
                 ~len:request.content_length;
               if request.content_length < Bytes.length client.state.body_bytes
               then Bytes.unsafe_set client.state.body_bytes request.content_length '\000');
             let body = client.state.body_string in
             compact_input client total;
             score_epoll_request config index client request body;
             if String.is_empty client.output && not client.close_after_output then loop ())))
  in
  loop ()

let epoll_interest client =
  let module Flags = Linux_ext.Epoll.Flags in
  if String.is_empty client.output then Flags.in_ else Flags.(in_ + out)

let set_epoll_interest epoll client =
  Linux_ext.Epoll.set epoll client.fd (epoll_interest client)

let remove_epoll_client epoll clients client =
  (try Linux_ext.Epoll.remove epoll client.fd with
   | _ -> ());
  Hashtbl.remove clients (fd_to_int client.fd);
  try Std_unix.close client.fd with
  | _ -> ()

let set_std_socket_options fd =
  try
    Std_unix.setsockopt fd Std_unix.TCP_NODELAY true;
    match Linux_ext.settcpopt_bool with
    | Error _ -> ()
    | Ok set_tcpopt_bool -> set_tcpopt_bool fd Linux_ext.TCP_QUICKACK true
  with
  | _ -> ()

let write_epoll_output epoll clients config index client =
  if not (String.is_empty client.output)
  then (
    let len = String.length client.output - client.output_pos in
    try
      let written =
        Std_unix.write_substring client.fd client.output client.output_pos len
      in
      if written = 0
      then remove_epoll_client epoll clients client
      else (
        client.output_pos <- client.output_pos + written;
        if client.output_pos = String.length client.output
        then (
          client.output <- "";
          client.output_pos <- 0;
          if client.close_after_output
          then remove_epoll_client epoll clients client
          else (
            process_epoll_input config index client;
            set_epoll_interest epoll client))
        else set_epoll_interest epoll client)
    with
    | Std_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
      set_epoll_interest epoll client
    | Std_unix.Unix_error _ -> remove_epoll_client epoll clients client)

let read_epoll_input epoll clients config index client =
  let rec loop () =
    ensure_input_capacity client;
    try
      let read =
        Std_unix.read
          client.fd
          client.input
          client.input_len
          (Bytes.length client.input - client.input_len)
      in
      if read = 0
      then remove_epoll_client epoll clients client
      else (
        client.input_len <- client.input_len + read;
        loop ())
    with
    | Std_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      process_epoll_input config index client;
      set_epoll_interest epoll client
    | Std_unix.Unix_error (EINTR, _, _) -> loop ()
    | Std_unix.Unix_error _ -> remove_epoll_client epoll clients client
  in
  loop ()

let accept_epoll_clients epoll clients listen_fd index =
  let rec loop () =
    try
      let fd, _addr = Std_unix.accept listen_fd in
      Std_unix.set_nonblock fd;
      set_std_socket_options fd;
      let client =
        { fd
        ; state = create_connection_state index
        ; input = Bytes.create 4096
        ; input_len = 0
        ; output = ""
        ; output_pos = 0
        ; close_after_output = false
        }
      in
      Hashtbl.set clients ~key:(fd_to_int fd) ~data:client;
      Linux_ext.Epoll.set epoll fd Linux_ext.Epoll.Flags.in_;
      loop ()
    with
    | Std_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
    | Std_unix.Unix_error (EINTR, _, _) -> loop ()
  in
  loop ()

let create_epoll_listen_fd socket_path tcp_port =
  match tcp_port with
  | Some port ->
    let fd = Std_unix.socket Std_unix.PF_INET Std_unix.SOCK_STREAM 0 in
    Std_unix.setsockopt fd Std_unix.SO_REUSEADDR true;
    Std_unix.bind fd (Std_unix.ADDR_INET (Std_unix.inet_addr_any, port));
    Std_unix.listen fd 4096;
    Std_unix.set_nonblock fd;
    fd, sprintf ":%d" port
  | None ->
    (try Std_unix.unlink socket_path with
     | _ -> ());
    let fd = Std_unix.socket Std_unix.PF_UNIX Std_unix.SOCK_STREAM 0 in
    Std_unix.bind fd (Std_unix.ADDR_UNIX socket_path);
    Std_unix.listen fd 4096;
    Std_unix.set_nonblock fd;
    Std_unix.chmod socket_path 0o666;
    fd, socket_path

let epoll_main () =
  let data_dir = Sys.getenv "DATA_DIR" |> Option.value ~default:"resources" in
  let socket_path =
    Sys.getenv "SOCKET_PATH" |> Option.value ~default:"/tmp/rinha-api.sock"
  in
  let tcp_port = Sys.getenv "TCP_PORT" |> Option.map ~f:Int.of_string in
  let config = Config.load data_dir in
  let index = Index.load data_dir in
  ensure_warmed index;
  let listen_fd, listen_label = create_epoll_listen_fd socket_path tcp_port in
  let epoll =
    match Linux_ext.Epoll.create with
    | Error error -> Error.raise error
    | Ok create -> create ~num_file_descrs:65_536 ~max_ready_events:1024
  in
  let clients = Hashtbl.create (module Int) in
  Linux_ext.Epoll.set epoll listen_fd Linux_ext.Epoll.Flags.in_;
  printf "rinha_api epoll transport listening on %s\n%!" listen_label;
  while true do
    ignore (Linux_ext.Epoll.wait epoll ~timeout:`Never : [ `Ok | `Timeout ]);
    Linux_ext.Epoll.iter_ready epoll ~f:(fun fd flags ->
      if fd_to_int fd = fd_to_int listen_fd
      then accept_epoll_clients epoll clients listen_fd index
      else (
        match Hashtbl.find clients (fd_to_int fd) with
        | None -> ()
        | Some client ->
          if Linux_ext.Epoll.Flags.do_intersect flags Linux_ext.Epoll.Flags.in_
          then read_epoll_input epoll clients config index client;
          if Hashtbl.mem clients (fd_to_int fd)
             && Linux_ext.Epoll.Flags.do_intersect flags Linux_ext.Epoll.Flags.out
          then write_epoll_output epoll clients config index client));
    Linux_ext.Epoll.Expert.clear_ready epoll
  done

let main () =
  let data_dir = Sys.getenv "DATA_DIR" |> Option.value ~default:"resources" in
  let socket_path =
    Sys.getenv "SOCKET_PATH" |> Option.value ~default:"/tmp/rinha-api.sock"
  in
  let config = Config.load data_dir in
  let index = Index.load data_dir in
  ensure_warmed index;
  let tcp_port = Sys.getenv "TCP_PORT" |> Option.map ~f:Int.of_string in
  let start_server ?socket where =
    Tcp.Server.create
      ~buffer_age_limit:`Unlimited
      ~backlog:4096
      ~max_accepts_per_batch:256
      ~max_connections:20_000
      ~reader_buffer_size:4096
      ~writer_buffer_size:512
      ~on_socket_accepted:set_socket_options
      ~on_handler_error:`Ignore
      ?socket
      where
      (fun _addr reader writer ->
        handle_connection config index (create_connection_state index) reader writer)
  in
  match tcp_port with
  | Some port ->
    let socket = tcp_reuseport_socket () in
    start_server ?socket (Tcp.Where_to_listen.of_port port)
    >>= fun _server -> Deferred.never ()
  | None ->
    (try Core_unix.unlink socket_path with
     | _ -> ());
    start_server (Tcp.Where_to_listen.of_file socket_path)
    >>= fun _server ->
    Core_unix.chmod socket_path ~perm:0o666;
    Deferred.never ()

let () =
  match Sys.getenv "RINHA_TRANSPORT" with
  | Some "epoll" -> epoll_main ()
  | _ ->
    Command_unix.run
      (Command.async
         ~summary:"Rinha 2026 API over a Unix domain socket"
         (Command.Param.return main))
