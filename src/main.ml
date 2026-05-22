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
  let server =
    Api_server.listen socket_path (Option.map int_of_string (Sys.getenv_opt "TCP_PORT"))
  in
  let env =
    { Api_server.config = config
    ; index
    ; model_only = bool_env "MODEL_ONLY"
    ; constant_only = bool_env "CONSTANT_ONLY"
    }
  in
  fork_workers (int_env "API_WORKERS" 1);
  Api_server.worker_loop env server
;;

let () = main ()
