open Core
open Async
open Rinha_lib

type request =
  { meth : string
  ; path : string
  ; content_length : int
  ; close : bool
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

let ensure_warmed =
  let warmed = ref false in
  fun index ->
    if not !warmed
    then (
      Index.prewarm index;
      warmed := true)

let parse_request_line line =
  match String.split line ~on:' ' with
  | meth :: path :: _ -> meth, path
  | _ -> failwith "bad request line"

let parse_header line =
  match String.lsplit2 line ~on:':' with
  | Some (name, value) when String.Caseless.equal name "content-length" ->
    `Content_length (String.strip value |> Int.of_string)
  | Some (name, value) when String.Caseless.equal name "connection" ->
    `Connection_close (String.Caseless.equal (String.strip value) "close")
  | _ -> `Other

let read_headers reader =
  Reader.read_line reader
  >>= function
  | `Eof -> return None
  | `Ok request_line ->
    let meth, path = parse_request_line request_line in
    let rec loop content_length close =
      Reader.read_line reader
      >>= function
      | `Eof -> return None
      | `Ok "" -> return (Some { meth; path; content_length; close })
      | `Ok line ->
        (match parse_header line with
         | `Content_length content_length -> loop content_length close
         | `Connection_close close_requested -> loop content_length (close || close_requested)
         | `Other -> loop content_length close)
    in
    loop 0 false

let read_body reader len =
  if len = 0
  then return (Some "")
  else (
    let bytes = Bytes.create len in
    Reader.really_read reader bytes
    >>| function
    | `Ok -> Some (Stdlib.Bytes.unsafe_to_string bytes)
    | `Eof _ -> None)

let write_and_flush writer payload =
  Writer.write writer payload;
  Writer.flushed writer

let write_and_close writer payload =
  write_and_flush writer payload
  >>= fun () -> Writer.close writer

let rec handle_connection config index reader writer =
  read_headers reader
  >>= function
  | None -> Writer.close writer
  | Some request ->
    read_body reader request.content_length
    >>= (function
     | None -> write_and_close writer bad_request_response
     | Some body ->
       (match request.meth, request.path with
        | "GET", "/ready" ->
          ensure_warmed index;
          let response = if request.close then ready_response_close else ready_response in
          write_response_or_continue config index reader writer request response
        | "POST", "/fraud-score" ->
          (try
            ensure_warmed index;
            let query = Vectorize.to_quantized config body in
            let frauds = Index.score_frauds index query in
            let response =
              if request.close
              then fraud_responses_close.(frauds)
              else fraud_responses.(frauds)
            in
            write_response_or_continue config index reader writer request response
          with
          | _ -> write_and_close writer bad_request_response)
        | _ ->
          let response =
            if request.close then not_found_response_close else not_found_response
          in
          write_response_or_continue config index reader writer request response))

and write_response_or_continue config index reader writer request payload =
  write_and_flush writer payload
  >>= fun () ->
  if request.close then Writer.close writer else handle_connection config index reader writer

let main () =
  let data_dir = Sys.getenv "DATA_DIR" |> Option.value ~default:"resources" in
  let socket_path =
    Sys.getenv "SOCKET_PATH" |> Option.value ~default:"/tmp/rinha-api.sock"
  in
  (try Core_unix.unlink socket_path with
   | _ -> ());
  let config = Config.load data_dir in
  let index = Index.load data_dir in
  let where = Tcp.Where_to_listen.of_file socket_path in
  Tcp.Server.create
    ~backlog:4096
    ~max_accepts_per_batch:256
    ~max_connections:20_000
    ~reader_buffer_size:4096
    ~writer_buffer_size:512
    ~on_handler_error:`Ignore
    where
    (fun _addr reader writer -> handle_connection config index reader writer)
  >>= fun _server ->
  Core_unix.chmod socket_path ~perm:0o666;
  Deferred.never ()

let () =
  Command_unix.run
    (Command.async ~summary:"Rinha 2026 API over a Unix domain socket" (Command.Param.return main))
