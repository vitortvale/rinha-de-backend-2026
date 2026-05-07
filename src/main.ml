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
  | Warmup
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
let warmup_response = ready_response
let warmup_response_close = ready_response_close

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
  else if starts_with line "GET /warmup " then Warmup
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
        | Warmup ->
          ensure_warmed index;
          let response =
            if request.close then warmup_response_close else warmup_response
          in
          write_response_or_continue config index state reader writer request response
        | Fraud_score ->
          (try
            ensure_warmed index;
            Vectorize.to_quantized_into config body state.query;
            let frauds =
              match Linear_model.decide state.query with
              | Fraud -> Index.k
              | Legit -> 0
              | Unknown -> Index.score_frauds_with_scorer index state.scorer state.query
            in
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
  Command_unix.run
    (Command.async ~summary:"Rinha 2026 API over a Unix domain socket" (Command.Param.return main))
