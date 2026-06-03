open Rinha_lib

let bool_env name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false
;;

let int_env name default =
  match Sys.getenv_opt name with
  | Some value -> max 1 (min 4 (int_of_string value))
  | None -> default
;;

let ensure_warmed =
  let warmed = ref false in
  fun index ->
    if not !warmed
    then (
      Index.prewarm index;
      warmed := true)
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
  let health_server =
    Api_server.listen socket_path (Option.map int_of_string (Sys.getenv_opt "TCP_PORT"))
  in
  let env =
    { Api_server.config = config
    ; index
    ; model_only = bool_env "MODEL_ONLY"
    ; constant_only = bool_env "CONSTANT_ONLY"
    }
  in
  match Sys.getenv_opt "FD_SOCKET_PATH" with
  | None ->
    (* Traditional mode: haproxy topology.  All forked workers compete for
       accept(2) on the shared unix socket — per-request load balancing. *)
    fork_workers (int_env "API_WORKERS" 1);
    Api_server.traditional_worker_loop env health_server
  | Some fd_socket_path ->
    (* fd-passing mode: OCaml LB topology.  Parent handles health checks;
       forked children do pure blocking recv_fd on the shared DGRAM socket. *)
    let fd_socket = Fd_pass.create_recv_fd_socket fd_socket_path in
    let parent_pid = Unix.getpid () in
    fork_workers (int_env "API_WORKERS" 1);
    if Unix.getpid () = parent_pid
    then Api_server.health_check_loop env health_server fd_socket
    else begin
      (try Unix.close health_server with _ -> ());
      Api_server.fd_worker_loop env fd_socket
    end
;;

let () = main ()
