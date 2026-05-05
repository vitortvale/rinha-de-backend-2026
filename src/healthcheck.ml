let socket_path =
  match Sys.getenv_opt "SOCKET_PATH" with
  | Some path -> path
  | None -> "/tmp/rinha-api.sock"

let request = "GET /ready HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"

let main () =
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.connect fd (Unix.ADDR_UNIX socket_path);
      ignore (Unix.write_substring fd request 0 (String.length request));
      let buf = Bytes.create 64 in
      let n = Unix.read fd buf 0 (Bytes.length buf) in
      if n >= 12 && String.equal (Bytes.sub_string buf 0 12) "HTTP/1.1 200"
      then 0
      else 1)

let () =
  try exit (main ()) with
  | _ -> exit 1
