open Core
open Async
open Rinha_lib

type request =
  { meth : string
  ; path : string
  ; content_length : int
  }

let response status reason ?(content_type = "text/plain") body =
  sprintf
    "HTTP/1.1 %d %s\r\nContent-Length: %d\r\nContent-Type: %s\r\nConnection: close\r\n\r\n%s"
    status
    reason
    (String.length body)
    content_type
    body

let response_json score =
  let approved = Float.(score < 0.6) in
  sprintf {|{"approved":%s,"fraud_score":%.1f}|} (Bool.to_string approved) score

let parse_request_line line =
  match String.split line ~on:' ' with
  | meth :: path :: _ -> meth, path
  | _ -> failwith "bad request line"

let header_content_length line =
  match String.lsplit2 line ~on:':' with
  | Some (name, value) when String.Caseless.equal name "content-length" ->
    Some (String.strip value |> Int.of_string)
  | _ -> None

let read_headers reader =
  Reader.read_line reader
  >>= function
  | `Eof -> return None
  | `Ok request_line ->
    let meth, path = parse_request_line request_line in
    let rec loop content_length =
      Reader.read_line reader
      >>= function
      | `Eof -> return None
      | `Ok "" -> return (Some { meth; path; content_length })
      | `Ok line ->
        let content_length =
          header_content_length line |> Option.value ~default:content_length
        in
        loop content_length
    in
    loop 0

let read_body reader len =
  if len = 0
  then return (Some "")
  else (
    let bytes = Bytes.create len in
    Reader.really_read reader bytes
    >>| function
    | `Ok -> Some (Bytes.to_string bytes)
    | `Eof _ -> None)

let write_and_close writer payload =
  Writer.write writer payload;
  Writer.close writer

let handle_request config index reader writer =
  read_headers reader
  >>= function
  | None -> Writer.close writer
  | Some request ->
    read_body reader request.content_length
    >>= (function
     | None -> write_and_close writer (response 400 "Bad Request" "bad request")
     | Some body ->
       (match request.meth, request.path with
        | "GET", "/ready" -> write_and_close writer (response 200 "OK" "OK")
        | "POST", "/fraud-score" ->
          Monitor.try_with (fun () ->
            let query = Vectorize.to_quantized config body in
            let score = Index.score index query in
            return (response_json score))
          >>= (function
           | Ok body ->
             write_and_close writer (response 200 "OK" ~content_type:"application/json" body)
           | Error _ -> write_and_close writer (response 400 "Bad Request" "bad request"))
        | _ -> write_and_close writer (response 404 "Not Found" "not found")))

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
    ~on_handler_error:`Ignore
    where
    (fun _addr reader writer -> handle_request config index reader writer)
  >>= fun _server ->
  Core_unix.chmod socket_path ~perm:0o666;
  Deferred.never ()

let () =
  Command_unix.run
    (Command.async ~summary:"Rinha 2026 API over a Unix domain socket" (Command.Param.return main))
