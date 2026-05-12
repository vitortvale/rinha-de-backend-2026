open Rinha_lib

type route =
  | Ready
  | Warmup
  | Fraud_score
  | Other

type request =
  { route : route
  ; content_length : int
  ; close : bool
  }

type connection =
  { fd : Unix.file_descr
  ; input : bytes
  ; query : int array
  ; scorer : Index.scorer
  ; model_only : bool
  ; constant_only : bool
  ; mutable pos : int
  ; mutable len : int
  ; mutable output : string
  ; mutable output_pos : int
  ; mutable close_after_write : bool
  }

exception Bad_request
exception Need_more

let response status reason ?content_type:_ ?(connection = "keep-alive") body =
  match connection with
  | "close" ->
    Printf.sprintf
      "HTTP/1.1 %d %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status
      reason
      (String.length body)
      body
  | _ ->
    Printf.sprintf
      "HTTP/1.1 %d %s\r\nContent-Length: %d\r\n\r\n%s"
      status
      reason
      (String.length body)
      body
;;

let response_json score =
  let approved = score < 0.6 in
  Printf.sprintf {|{"approved":%s,"fraud_score":%.1f}|} (string_of_bool approved) score
;;

let static_response ?connection status reason ?(content_type = "text/plain") body =
  response status reason ~content_type ?connection body
;;

let ready_response = static_response 200 "OK" "OK"
let ready_response_close = static_response ~connection:"close" 200 "OK" "OK"
let warmup_response = ready_response
let warmup_response_close = ready_response_close
let bad_request_response = static_response ~connection:"close" 400 "Bad Request" "bad request"
let not_found_response = static_response 404 "Not Found" "not found"
let not_found_response_close = static_response ~connection:"close" 404 "Not Found" "not found"

let fraud_responses =
  Array.init (Index.k + 1) (fun frauds ->
    let score = float_of_int frauds /. float_of_int Index.k in
    static_response 200 "OK" ~content_type:"application/json" (response_json score))
;;

let fraud_responses_close =
  Array.init (Index.k + 1) (fun frauds ->
    let score = float_of_int frauds /. float_of_int Index.k in
    static_response
      200
      "OK"
      ~connection:"close"
      ~content_type:"application/json"
      (response_json score))
;;

let ensure_warmed =
  let warmed = ref false in
  fun index ->
    if not !warmed
    then (
      Index.prewarm index;
      warmed := true)
;;

let ascii_lower_code c =
  let code = Char.code c in
  if code >= Char.code 'A' && code <= Char.code 'Z' then code + 32 else code
;;

let starts_with_bytes b start stop prefix =
  let prefix_len = String.length prefix in
  stop - start >= prefix_len
  &&
  let rec loop i =
    i = prefix_len || (Char.equal (Bytes.unsafe_get b (start + i)) prefix.[i] && loop (i + 1))
  in
  loop 0
;;

let parse_request_line b start stop =
  if starts_with_bytes b start stop "POST /fraud-score "
  then Fraud_score
  else if starts_with_bytes b start stop "GET /warmup "
  then Warmup
  else if starts_with_bytes b start stop "GET /ready "
  then Ready
  else Other
;;

let caseless_equal_at b start stop prefix =
  let prefix_len = String.length prefix in
  stop - start >= prefix_len
  &&
  let rec loop i =
    i = prefix_len
    || (ascii_lower_code (Bytes.unsafe_get b (start + i)) = Char.code prefix.[i]
        && loop (i + 1))
  in
  loop 0
;;

let skip_header_ws b i stop =
  let rec loop i =
    if i < stop
       && (Char.equal (Bytes.unsafe_get b i) ' ' || Char.equal (Bytes.unsafe_get b i) '\t')
    then loop (i + 1)
    else i
  in
  loop i
;;

let parse_header_int b i stop =
  let rec loop i acc =
    if i < stop
    then (
      let c = Bytes.unsafe_get b i in
      if Char.(c >= '0' && c <= '9')
      then loop (i + 1) ((acc * 10) + Char.code c - Char.code '0')
      else acc)
    else acc
  in
  loop (skip_header_ws b i stop) 0
;;

let value_is_close b i stop =
  let i = skip_header_ws b i stop in
  i + 5 <= stop
  && ascii_lower_code (Bytes.unsafe_get b i) = Char.code 'c'
  && ascii_lower_code (Bytes.unsafe_get b (i + 1)) = Char.code 'l'
  && ascii_lower_code (Bytes.unsafe_get b (i + 2)) = Char.code 'o'
  && ascii_lower_code (Bytes.unsafe_get b (i + 3)) = Char.code 's'
  && ascii_lower_code (Bytes.unsafe_get b (i + 4)) = Char.code 'e'
;;

let next_line b i stop =
  let rec loop j =
    if j >= stop
    then stop, stop
    else if Char.equal (Bytes.unsafe_get b j) '\r'
            && j + 1 < stop
            && Char.equal (Bytes.unsafe_get b (j + 1)) '\n'
    then j, j + 2
    else loop (j + 1)
  in
  loop i
;;

let scan_request conn =
  let b = conn.input in
  let line_stop, next = next_line b conn.pos conn.len in
  if line_stop = conn.len then raise Need_more;
  let route = parse_request_line b conn.pos line_stop in
  let rec loop i content_length close =
    if i + 1 >= conn.len
    then raise Need_more
    else if Char.equal (Bytes.unsafe_get b i) '\r'
            && Char.equal (Bytes.unsafe_get b (i + 1)) '\n'
    then { route; content_length; close }, i + 2
    else if Char.equal (Bytes.unsafe_get b i) '\n'
    then { route; content_length; close }, i + 1
    else (
      let line_stop, next = next_line b i conn.len in
      if line_stop = conn.len then raise Need_more;
      if caseless_equal_at b i line_stop "content-length:"
      then loop next (parse_header_int b (i + 15) line_stop) close
      else if caseless_equal_at b i line_stop "connection:"
      then loop next content_length (close || value_is_close b (i + 11) line_stop)
      else loop next content_length close)
  in
  loop next 0 false
;;

let compact_input conn =
  if conn.pos > 0
  then (
    let remaining = conn.len - conn.pos in
    Bytes.blit conn.input conn.pos conn.input 0 remaining;
    conn.pos <- 0;
    conn.len <- remaining)
;;

let score_frauds index conn =
  if conn.model_only
  then Linear_model.decide_probability_bucket conn.query
  else (
    let frauds = Linear_model.decide conn.query in
    if frauds >= 0
    then frauds
    else Index.score_frauds_with_scorer index conn.scorer conn.query)
;;

let has_pending_output conn = conn.output_pos < String.length conn.output

let set_output conn payload close_after_write =
  conn.output <- payload;
  conn.output_pos <- 0;
  conn.close_after_write <- close_after_write
;;

let process_one_request config index conn =
  let request, body_start = scan_request conn in
  let body_stop = body_start + request.content_length in
  if request.content_length < 0 || body_stop > Bytes.length conn.input then raise Bad_request;
  if conn.len < body_stop then raise Need_more;
  conn.pos <- body_start;
  let payload =
    match request.route with
    | Ready ->
      if request.content_length > 0 then conn.pos <- body_stop;
      ensure_warmed index;
      if request.close then ready_response_close else ready_response
    | Warmup ->
      if request.content_length > 0 then conn.pos <- body_stop;
      ensure_warmed index;
      if request.close then warmup_response_close else warmup_response
    | Fraud_score ->
      conn.pos <- body_stop;
      let frauds =
        if conn.constant_only
        then 0
        else (
          Vectorize.to_quantized_bytes_into
            config
            conn.input
            body_start
            request.content_length
            conn.query;
          score_frauds index conn)
      in
      if request.close then fraud_responses_close.(frauds) else fraud_responses.(frauds)
    | Other ->
      conn.pos <- body_stop;
      if request.close then not_found_response_close else not_found_response
  in
  set_output conn payload request.close
;;

let process_ready_requests config index conn =
  let rec loop () =
    if has_pending_output conn
    then ()
    else (
      try
        process_one_request config index conn;
        loop ()
      with
      | Need_more -> compact_input conn
      | Bad_request ->
        set_output conn bad_request_response true;
        conn.pos <- conn.len)
  in
  loop ()
;;

let would_block = function
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> true
  | _ -> false
;;

let close_quietly fd =
  try Unix.close fd with
  | _ -> ()
;;

let remove_connection conns fd =
  close_quietly fd;
  Hashtbl.remove conns fd
;;

let rec write_available conns conn =
  let rec loop () =
    if has_pending_output conn
    then (
      let written =
        Unix.write_substring
          conn.fd
          conn.output
          conn.output_pos
          (String.length conn.output - conn.output_pos)
      in
      if written = 0
      then raise End_of_file
      else (
        conn.output_pos <- conn.output_pos + written;
        loop ()))
    else if conn.close_after_write
    then remove_connection conns conn.fd
  in
  try loop () with
  | exn when would_block exn -> ()
  | Unix.Unix_error _ | End_of_file -> remove_connection conns conn.fd
;;

let read_available config index conns conn =
  let rec loop () =
    compact_input conn;
    if conn.len = Bytes.length conn.input
    then raise Bad_request
    else (
      match Unix.read conn.fd conn.input conn.len (Bytes.length conn.input - conn.len) with
      | 0 -> remove_connection conns conn.fd
      | n ->
        conn.len <- conn.len + n;
        if not (has_pending_output conn) then process_ready_requests config index conn;
        if has_pending_output conn then write_available conns conn;
        if not (has_pending_output conn) then loop ())
  in
  try loop () with
  | exn when would_block exn -> ()
  | Bad_request ->
    set_output conn bad_request_response true;
    conn.pos <- conn.len
  | Unix.Unix_error _ | End_of_file -> remove_connection conns conn.fd
;;

let constant_only_enabled () =
  match Sys.getenv_opt "CONSTANT_ONLY" with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false
;;

let create_connection index model_only constant_only fd =
  { fd
  ; input = Bytes.create 8192
  ; query = Array.make Vectorize.dim 0
  ; scorer = Index.create_scorer index
  ; model_only
  ; constant_only
  ; pos = 0
  ; len = 0
  ; output = ""
  ; output_pos = 0
  ; close_after_write = false
  }
;;

let configure_client_fd fd addr =
  Unix.set_nonblock fd;
  match addr with
  | Unix.ADDR_INET _ ->
    (try Unix.setsockopt fd Unix.TCP_NODELAY true with
     | _ -> ())
  | Unix.ADDR_UNIX _ -> ()
;;

let accept_available index model_only constant_only conns server =
  let rec loop () =
    let fd, addr = Unix.accept server in
    configure_client_fd fd addr;
    Hashtbl.replace conns fd (create_connection index model_only constant_only fd);
    loop ()
  in
  try loop () with
  | exn when would_block exn -> ()
;;

let listen_socket socket_path tcp_port =
  let domain, addr =
    match tcp_port with
    | Some port -> Unix.PF_INET, Unix.ADDR_INET (Unix.inet_addr_any, port)
    | None -> Unix.PF_UNIX, Unix.ADDR_UNIX socket_path
  in
  (match tcp_port with
   | Some _ -> ()
   | None ->
     (try Unix.unlink socket_path with
      | _ -> ()));
  let fd = Unix.socket domain Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd addr;
  (match tcp_port with
   | Some _ -> ()
   | None -> Unix.chmod socket_path 0o666);
  Unix.listen fd 4096;
  Unix.set_nonblock fd;
  fd
;;

let model_only_enabled () =
  match Sys.getenv_opt "MODEL_ONLY" with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false
;;

let worker_count () =
  match Sys.getenv_opt "API_WORKERS" with
  | Some value -> Int.max 1 (Int.min 4 (int_of_string value))
  | None -> 1
;;

let rec reap_children () =
  match Unix.waitpid [ Unix.WNOHANG ] (-1) with
  | 0, _ -> ()
  | _ -> reap_children ()
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
;;

let fork_workers count =
  let rec loop remaining =
    if remaining <= 1
    then ()
    else (
      match Unix.fork () with
      | 0 -> ()
      | _pid -> loop (remaining - 1))
  in
  loop count
;;

let main () =
  Sys.Safe.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.Safe.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> reap_children ()));
  let data_dir = Option.value (Sys.getenv_opt "DATA_DIR") ~default:"resources" in
  let socket_path =
    Option.value (Sys.getenv_opt "SOCKET_PATH") ~default:"/tmp/rinha-api.sock"
  in
  let config = Config.load data_dir in
  let index = Index.load data_dir in
  ensure_warmed index;
  let model_only = model_only_enabled () in
  let constant_only = constant_only_enabled () in
  let tcp_port = Option.map int_of_string (Sys.getenv_opt "TCP_PORT") in
  let server = listen_socket socket_path tcp_port in
  fork_workers (worker_count ());
  let conns = Hashtbl.create 4096 in
  while true do
    let read_fds = ref [ server ] in
    let write_fds = ref [] in
    Hashtbl.iter
      (fun fd conn ->
        if has_pending_output conn then write_fds := fd :: !write_fds;
        if not conn.close_after_write then read_fds := fd :: !read_fds)
      conns;
    let readable, writable, _ = Unix.select !read_fds !write_fds [] (-1.0) in
    List.iter
      (fun fd ->
        if fd = server
        then accept_available index model_only constant_only conns server
        else (
          match Hashtbl.find_opt conns fd with
          | Some conn -> read_available config index conns conn
          | None -> ()))
      readable;
    List.iter
      (fun fd ->
        match Hashtbl.find_opt conns fd with
        | Some conn ->
          write_available conns conn;
          (match Hashtbl.find_opt conns fd with
           | Some conn when (not (has_pending_output conn)) && not conn.close_after_write ->
             process_ready_requests config index conn
           | _ -> ())
        | None -> ())
      writable
  done
;;

let () = main ()
