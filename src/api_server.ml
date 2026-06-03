type env =
  { config : Config.t
  ; index : Index.t
  ; model_only : bool
  ; constant_only : bool
  }

type worker =
  { input : bytes
  ; query : int array
  }

let input_size = 8192

let would_block = function
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> true
  | _ -> false
;;

let close_quietly fd =
  try Unix.close fd with
  | _ -> ()
;;

let write_all fd payload =
  let len = String.length payload in
  let rec loop pos =
    if pos < len
    then (
      let written = Unix.write_substring fd payload pos (len - pos) in
      if written = 0 then raise End_of_file;
      loop (pos + written))
  in
  loop 0
;;

let read_request fd buf =
  let cap = Bytes.length buf in
  let rec loop len =
    let request = Api_http.scan_request buf len in
    let body_stop = request.body_start + request.content_length in
    if request.content_length < 0 || body_stop > cap
    then raise Api_http.Bad_request
    else if len >= body_stop
    then request
    else (
      let n = Unix.read fd buf len (body_stop - len) in
      if n = 0 then raise End_of_file;
      loop (len + n))
  in
  let rec read_headers len =
    try loop len with
    | Api_http.Need_more ->
      if len = cap
      then raise Api_http.Bad_request
      else (
        let n = Unix.read fd buf len (cap - len) in
        if n = 0 then raise End_of_file;
        read_headers (len + n))
  in
  read_headers 0
;;

(* Use the stack-based scoring path: it sorts top_nprobe centroids once,
   then fast-exits after fast_nprobe probes for clear cases (0 or k/k
   fraud neighbors), and only scans the full nprobe for ambiguous cases.
   No per-request heap allocation; stack arrays are reused by the runtime. *)
let[@inline always] score_frauds model_only index query =
  if model_only
  then Model.decide_probability_bucket query
  else (
    let frauds = Model.decide query in
    if frauds >= 0 then frauds else Index.score_frauds index query)
;;

let handle_request env worker request =
  match request.Api_http.route with
  | Ready | Warmup -> Api_http.ready_response
  | Other -> Api_http.not_found_response
  | Fraud_score ->
    let query = worker.query in
    let frauds =
      if env.constant_only
      then 0
      else (
        Vectorize.to_quantized_bytes_into
          env.config
          worker.input
          request.body_start
          request.content_length
          query;
        score_frauds env.model_only env.index query)
    in
    Api_http.fraud_responses.(frauds)
;;

let handle_client env worker fd =
  try
    let request = read_request fd worker.input in
    write_all fd (handle_request env worker request);
    close_quietly fd
  with
  | Api_http.Bad_request ->
    (try write_all fd Api_http.bad_request_response with
     | _ -> ());
    close_quietly fd
  | _ -> close_quietly fd
;;

let create_worker _index =
  { input = Bytes.create input_size; query = Array.make Vectorize.dim 0 }
;;

(* Health-check only loop for the parent process.  Responds to GET /warmup
   so Docker considers the container healthy.  Runs select with a short
   timeout so it can also drain any excess client fds when idle. *)
let health_check_loop env health_server fd_socket =
  let worker = create_worker env.index in
  let watch = [ health_server; fd_socket ] in
  while true do
    (try
       let ready, _, _ = Unix.select watch [] [] 5.0 in
       List.iter
         (fun rfd ->
           if rfd = health_server
           then (
             try
               let fd, _ = Unix.accept health_server in
               handle_client env worker fd
             with
             | Unix.Unix_error _ -> ())
           else (
             match Fd_pass.recv_fd_nonblock fd_socket with
             | Some fd -> handle_client env worker fd
             | None -> ()))
         ready
     with
     | Unix.Unix_error _ -> ())
  done
;;

(* Traditional unix-socket accept loop (haproxy topology).  Each forked
   worker competes for accept(2) on the shared unix socket — natural
   per-request load balancing with no fd-passing overhead. *)
let traditional_worker_loop env server =
  let worker = create_worker env.index in
  while true do
    (try
       let fd, addr = Unix.accept server in
       (match addr with
        | Unix.ADDR_INET _ -> (try Unix.setsockopt fd Unix.TCP_NODELAY true with _ -> ())
        | Unix.ADDR_UNIX _ -> ());
       handle_client env worker fd
     with
     | Unix.Unix_error _ -> ())
  done
;;

(* Pure fd-receiver loop for forked worker processes.  Blocking recvmsg
   on a shared DGRAM socket: the kernel delivers each datagram to exactly
   one waiting process, so there is no thundering-herd and no select(2)
   overhead on the hot path. *)
let fd_worker_loop env fd_socket =
  let worker = create_worker env.index in
  while true do
    (try
       let fd = Fd_pass.recv_fd fd_socket in
       handle_client env worker fd
     with
     | Unix.Unix_error _ -> ())
  done
;;

let listen socket_path tcp_port =
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
  fd
;;
