module Config = struct
  let int_env name default =
    match Sys.getenv_opt name with
    | Some v -> (try int_of_string v with _ -> default)
    | None -> default
  ;;

  let upstreams () =
    match Sys.getenv_opt "UPSTREAMS" with
    | Some v ->
      v
      |> String.split_on_char ','
      |> List.filter_map (fun s ->
        let s = String.trim s in
        if String.length s = 0 then None else Some s)
      |> Array.of_list
    | None -> [| "/sockets/api1.fd.sock"; "/sockets/api2.fd.sock" |]
  ;;

  let load () =
    let ups = upstreams () in
    if Array.length ups = 0 then failwith "UPSTREAMS is empty";
    int_env "PORT" 9999, ups, max 1 (int_env "LB_WORKERS" 32)
  ;;
end

let listen_tcp port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.setsockopt fd Unix.SO_REUSEPORT true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen fd 65535;
  fd
;;

let rec reap_children () =
  match Unix.waitpid [ Unix.WNOHANG ] (-1) with
  | 0, _ -> ()
  | _ -> reap_children ()
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
;;

let fork_workers count =
  let rec loop n =
    if n <= 1
    then ()
    else (
      match Unix.fork () with
      | 0 -> ()
      | _pid -> loop (n - 1))
  in
  loop count
;;

(*
 * Per-worker state.  The DGRAM send socket is created once per process
 * and reused for every request — no socket allocation on the hot path.
 *)
type worker =
  { upstreams : string array
  ; send_sock : Unix.file_descr
  ; mutable next : int
  }

let create_worker upstreams =
  { upstreams
  ; send_sock = Fd_pass.create_send_socket ()
  ; next = Unix.getpid () mod Array.length upstreams
  }
;;

(* Round-robin upstream selection — zero alloc. *)
let[@inline always] [@zero_alloc] pick worker =
  let i = worker.next in
  worker.next <- (i + 1) mod Array.length worker.upstreams;
  Array.unsafe_get worker.upstreams i
;;

(*
 * Hot path: accept TCP connection, set TCP_NODELAY, pass the fd directly
 * to the chosen upstream API worker via SCM_RIGHTS, then close our copy.
 * The LB never reads or writes a single byte of HTTP data.
 *)
let worker_loop server worker =
  while true do
    match Unix.accept server with
    | client, _ ->
      (try Unix.setsockopt client Unix.TCP_NODELAY true with _ -> ());
      (try Fd_pass.send_fd worker.send_sock (pick worker) client with _ -> ());
      (try Unix.close client with _ -> ())
    | exception Unix.Unix_error _ -> ()
  done
;;

let main () =
  Sys.Safe.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.Safe.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> reap_children ()));
  let port, upstreams, n_workers = Config.load () in
  let server = listen_tcp port in
  fork_workers n_workers;
  worker_loop server (create_worker upstreams)
;;

let () = main ()
