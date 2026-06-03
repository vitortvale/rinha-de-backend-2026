type env =
  { config : Config.t
  ; index : Index.t
  ; model_only : bool
  ; constant_only : bool
  }

type worker =
  { input : bytes
  ; query : int array
  ; scorer : Index.scorer
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

let[@inline always] score_frauds model_only index scorer query =
  if model_only
  then Model.decide_probability_bucket query
  else (
    let frauds = Model.decide query in
    if frauds >= 0
    then frauds
    else (
      let frauds = Index.score_frauds_with_scorer index scorer query in
      frauds))
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
        score_frauds env.model_only env.index worker.scorer query)
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

let create_worker index =
  { input = Bytes.create input_size
  ; query = Array.make Vectorize.dim 0
  ; scorer = Index.create_scorer index
  }
;;

(*
 * Dual-mode worker loop.
 *
 * fd_socket — DGRAM Unix socket that receives client file descriptors from the
 *             OCaml LB via SCM_RIGHTS.  All forked API workers share the same
 *             socket; the kernel delivers each datagram to exactly one waiter.
 *
 * health_server — the original Unix STREAM socket used exclusively by the
 *                 Docker health-check binary (GET /warmup).
 *
 * select(2) multiplexes between the two so health checks still work while the
 * bulk of traffic arrives as forwarded fds.
 *)
let worker_loop env health_server fd_socket =
  let worker = create_worker env.index in
  let watch = [ health_server; fd_socket ] in
  while true do
    (try
       let ready, _, _ = Unix.select watch [] [] (-1.0) in
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
             (* Non-blocking: if another worker raced ahead and consumed the
                datagram, recv_fd_nonblock returns -1 and we loop back. *)
             (match Fd_pass.recv_fd_nonblock fd_socket with
              | Some fd -> handle_client env worker fd
              | None -> ())))
         ready
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
