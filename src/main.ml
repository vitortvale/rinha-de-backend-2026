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
  }

let response status reason ?(content_type = "text/plain") ?(connection = "keep-alive") body =
  Printf.sprintf
    "HTTP/1.1 %d %s\r\nContent-Length: %d\r\nContent-Type: %s\r\nConnection: \
     %s\r\n\r\n%s"
    status
    reason
    (String.length body)
    content_type
    connection
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

let create_connection_state index =
  { query = Array.make Vectorize.dim 0; scorer = Index.create_scorer index }
;;

let ensure_warmed =
  let warmed = ref false in
  fun index ->
    if not !warmed
    then (
      Index.prewarm index;
      warmed := true)
;;

let trim_cr line =
  let len = String.length line in
  if len > 0 && Char.equal line.[len - 1] '\r' then String.sub line 0 (len - 1) else line
;;

let starts_with line prefix =
  let line_len = String.length line in
  let prefix_len = String.length prefix in
  line_len >= prefix_len
  &&
  let rec loop i =
    i = prefix_len || (Char.equal line.[i] prefix.[i] && loop (i + 1))
  in
  loop 0
;;

let parse_request_line line =
  if starts_with line "POST /fraud-score " then Fraud_score
  else if starts_with line "GET /warmup " then Warmup
  else if starts_with line "GET /ready " then Ready
  else Other
;;

let ascii_lower_code c =
  let code = Char.code c in
  if code >= Char.code 'A' && code <= Char.code 'Z' then code + 32 else code
;;

let caseless_equal_at line prefix =
  let line_len = String.length line in
  let prefix_len = String.length prefix in
  line_len >= prefix_len
  &&
  let rec loop i =
    i = prefix_len || (ascii_lower_code line.[i] = Char.code prefix.[i] && loop (i + 1))
  in
  loop 0
;;

let skip_header_ws line i =
  let len = String.length line in
  let rec loop i =
    if i < len && (Char.equal line.[i] ' ' || Char.equal line.[i] '\t')
    then loop (i + 1)
    else i
  in
  loop i
;;

let parse_header_int line i =
  let len = String.length line in
  let rec loop i acc =
    if i < len
    then (
      let c = line.[i] in
      if Char.(c >= '0' && c <= '9')
      then loop (i + 1) ((acc * 10) + Char.code c - Char.code '0')
      else acc)
    else acc
  in
  loop (skip_header_ws line i) 0
;;

let value_is_close line i =
  let i = skip_header_ws line i in
  i + 5 <= String.length line
  && ascii_lower_code line.[i] = Char.code 'c'
  && ascii_lower_code line.[i + 1] = Char.code 'l'
  && ascii_lower_code line.[i + 2] = Char.code 'o'
  && ascii_lower_code line.[i + 3] = Char.code 's'
  && ascii_lower_code line.[i + 4] = Char.code 'e'
;;

let parse_header line =
  if caseless_equal_at line "content-length:"
  then `Content_length (parse_header_int line 15)
  else if caseless_equal_at line "connection:"
  then `Connection_close (value_is_close line 11)
  else `Other
;;

let read_headers reader =
  try
    let route = parse_request_line (trim_cr (Eio.Buf_read.line reader)) in
    let rec loop content_length close =
      match trim_cr (Eio.Buf_read.line reader) with
      | "" -> Some { route; content_length; close }
      | line ->
        (match parse_header line with
         | `Content_length content_length -> loop content_length close
         | `Connection_close close_requested -> loop content_length (close || close_requested)
         | `Other -> loop content_length close)
    in
    loop 0 false
  with
  | End_of_file -> None
;;

let read_body reader len =
  if len = 0 then Some "" else try Some (Eio.Buf_read.take len reader) with End_of_file -> None
;;

let write_and_close flow payload =
  Eio.Flow.copy_string payload flow;
  Eio.Flow.close flow
;;

let score_frauds index state =
  match Linear_model.decide state.query with
  | Fraud -> Index.k
  | Legit -> 0
  | Unknown -> Index.score_frauds_with_scorer index state.scorer state.query
;;

let rec handle_connection config index state reader flow =
  match read_headers reader with
  | None -> Eio.Flow.close flow
  | Some request ->
    (match read_body reader request.content_length with
     | None -> write_and_close flow bad_request_response
     | Some body ->
       (match request.route with
        | Ready ->
          ensure_warmed index;
          let response = if request.close then ready_response_close else ready_response in
          write_response_or_continue config index state reader flow request response
        | Warmup ->
          ensure_warmed index;
          let response =
            if request.close then warmup_response_close else warmup_response
          in
          write_response_or_continue config index state reader flow request response
        | Fraud_score ->
          (try
            ensure_warmed index;
            Vectorize.to_quantized_into config body state.query;
            let frauds = score_frauds index state in
            let response =
              if request.close
              then fraud_responses_close.(frauds)
              else fraud_responses.(frauds)
            in
            write_response_or_continue config index state reader flow request response
          with
          | _ -> write_and_close flow bad_request_response)
        | Other ->
          let response =
            if request.close then not_found_response_close else not_found_response
          in
          write_response_or_continue config index state reader flow request response))

and write_response_or_continue config index state reader flow request payload =
  Eio.Flow.copy_string payload flow;
  if request.close then Eio.Flow.close flow else handle_connection config index state reader flow
;;

let serve_connection config index flow _addr =
  let reader = Eio.Buf_read.of_flow flow ~initial_size:4096 ~max_size:262_144 in
  handle_connection config index (create_connection_state index) reader flow
;;

let listen_addr socket_path tcp_port =
  match tcp_port with
  | Some port -> `Tcp (Eio.Net.Ipaddr.of_raw "\000\000\000\000", port)
  | None -> `Unix socket_path
;;

let main env =
  let data_dir = Option.value (Sys.getenv_opt "DATA_DIR") ~default:"resources" in
  let socket_path =
    Option.value (Sys.getenv_opt "SOCKET_PATH") ~default:"/tmp/rinha-api.sock"
  in
  let config = Config.load data_dir in
  let index = Index.load data_dir in
  ensure_warmed index;
  let tcp_port = Option.map int_of_string (Sys.getenv_opt "TCP_PORT") in
  Eio.Switch.run
  @@ fun sw ->
  let addr = listen_addr socket_path tcp_port in
  (match tcp_port with
   | Some _ -> ()
   | None ->
     (try Unix.unlink socket_path with
      | _ -> ()));
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~reuse_port:(Option.is_some tcp_port)
      ~backlog:4096
      env#net
      addr
  in
  (match tcp_port with
   | Some _ -> ()
   | None -> Unix.chmod socket_path 0o666);
  Eio.Net.run_server
    ~max_connections:20_000
    ~on_error:(fun _ -> ())
    socket
    (serve_connection config index)
;;

let () = Eio_main.run main
