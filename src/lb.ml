module Std_unix = Unix

open Core

type side =
  | Client
  | Upstream

type conn =
  { client : Std_unix.file_descr
  ; upstream : Std_unix.file_descr
  ; mutable c2u : Bytes.t
  ; mutable c2u_pos : int
  ; mutable c2u_len : int
  ; mutable u2c : Bytes.t
  ; mutable u2c_pos : int
  ; mutable u2c_len : int
  }

type endpoint =
  { conn : conn
  ; side : side
  }

let fd_to_int (fd : Std_unix.file_descr) : int = Obj.magic fd

let set_nonblock fd =
  try Std_unix.set_nonblock fd with
  | _ -> ()

let close_noerr fd =
  try Std_unix.close fd with
  | _ -> ()

let set_tcp_options fd =
  try
    Std_unix.setsockopt fd Std_unix.TCP_NODELAY true;
    match Linux_ext.settcpopt_bool with
    | Error _ -> ()
    | Ok set_tcpopt_bool -> set_tcpopt_bool fd Linux_ext.TCP_QUICKACK true
  with
  | _ -> ()

let has_pending conn = conn.c2u_len > 0 || conn.u2c_len > 0

let remove_conn epoll endpoints conn =
  (try Linux_ext.Epoll.remove epoll conn.client with
   | _ -> ());
  (try Linux_ext.Epoll.remove epoll conn.upstream with
   | _ -> ());
  Hashtbl.remove endpoints (fd_to_int conn.client);
  Hashtbl.remove endpoints (fd_to_int conn.upstream);
  close_noerr conn.client;
  close_noerr conn.upstream

let interest endpoint =
  let module Flags = Linux_ext.Epoll.Flags in
  match endpoint.side with
  | Client ->
    let flags = if endpoint.conn.c2u_len = 0 then Flags.in_ else Flags.none in
    if endpoint.conn.u2c_len > 0 then Flags.(flags + out) else flags
  | Upstream ->
    let flags = if endpoint.conn.u2c_len = 0 then Flags.in_ else Flags.none in
    if endpoint.conn.c2u_len > 0 then Flags.(flags + out) else flags

let set_interest epoll endpoint = Linux_ext.Epoll.set epoll (match endpoint.side with
  | Client -> endpoint.conn.client
  | Upstream -> endpoint.conn.upstream)
  (interest endpoint)

let refresh epoll endpoints conn =
  match Hashtbl.find endpoints (fd_to_int conn.client), Hashtbl.find endpoints (fd_to_int conn.upstream) with
  | Some client_ep, Some upstream_ep ->
    set_interest epoll client_ep;
    set_interest epoll upstream_ep
  | _ -> ()

let read_into epoll endpoints endpoint =
  let conn = endpoint.conn in
  let fd, buffer_ref, pos_ref, len_ref =
    match endpoint.side with
    | Client -> conn.client, (fun () -> conn.c2u), (fun v -> conn.c2u_pos <- v), (fun v -> conn.c2u_len <- v)
    | Upstream -> conn.upstream, (fun () -> conn.u2c), (fun v -> conn.u2c_pos <- v), (fun v -> conn.u2c_len <- v)
  in
  let buffer = buffer_ref () in
  try
    let n = Std_unix.read fd buffer 0 (Bytes.length buffer) in
    if n = 0
    then remove_conn epoll endpoints conn
    else (
      pos_ref 0;
      len_ref n;
      refresh epoll endpoints conn)
  with
  | Std_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> refresh epoll endpoints conn
  | Std_unix.Unix_error _ -> remove_conn epoll endpoints conn

let write_from epoll endpoints endpoint =
  let conn = endpoint.conn in
  let fd, buffer, pos, len, set_pos, set_len =
    match endpoint.side with
    | Client ->
      ( conn.client
      , conn.u2c
      , conn.u2c_pos
      , conn.u2c_len
      , (fun v -> conn.u2c_pos <- v)
      , fun v -> conn.u2c_len <- v )
    | Upstream ->
      ( conn.upstream
      , conn.c2u
      , conn.c2u_pos
      , conn.c2u_len
      , (fun v -> conn.c2u_pos <- v)
      , fun v -> conn.c2u_len <- v )
  in
  if len > 0
  then (
    try
      let n = Std_unix.write fd buffer pos len in
      if n = 0
      then remove_conn epoll endpoints conn
      else if n = len
      then (
        set_pos 0;
        set_len 0;
        refresh epoll endpoints conn)
      else (
        set_pos (pos + n);
        set_len (len - n);
        refresh epoll endpoints conn)
    with
    | Std_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
      refresh epoll endpoints conn
    | Std_unix.Unix_error _ -> remove_conn epoll endpoints conn)

let connect_upstream path =
  let fd = Std_unix.socket Std_unix.PF_UNIX Std_unix.SOCK_STREAM 0 in
  Std_unix.connect fd (Std_unix.ADDR_UNIX path);
  set_nonblock fd;
  fd

let accept_loop epoll endpoints listen_fd upstreams next_upstream =
  let rec loop () =
    try
      let client, _ = Std_unix.accept listen_fd in
      set_nonblock client;
      set_tcp_options client;
      let upstream_path = upstreams.(!next_upstream) in
      next_upstream := (!next_upstream + 1) mod Array.length upstreams;
      let upstream = connect_upstream upstream_path in
      let conn =
        { client
        ; upstream
        ; c2u = Bytes.create 8192
        ; c2u_pos = 0
        ; c2u_len = 0
        ; u2c = Bytes.create 8192
        ; u2c_pos = 0
        ; u2c_len = 0
        }
      in
      let client_ep = { conn; side = Client } in
      let upstream_ep = { conn; side = Upstream } in
      Hashtbl.set endpoints ~key:(fd_to_int client) ~data:client_ep;
      Hashtbl.set endpoints ~key:(fd_to_int upstream) ~data:upstream_ep;
      set_interest epoll client_ep;
      set_interest epoll upstream_ep;
      loop ()
    with
    | Std_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
    | Std_unix.Unix_error (EINTR, _, _) -> loop ()
  in
  loop ()

let listen_fd port =
  let fd = Std_unix.socket Std_unix.PF_INET Std_unix.SOCK_STREAM 0 in
  Std_unix.setsockopt fd Std_unix.SO_REUSEADDR true;
  Std_unix.bind fd (Std_unix.ADDR_INET (Std_unix.inet_addr_any, port));
  Std_unix.listen fd 4096;
  set_nonblock fd;
  fd

let () =
  let port = Sys.getenv "LB_PORT" |> Option.value_map ~default:9999 ~f:Int.of_string in
  let upstreams =
    Sys.getenv "UPSTREAM_SOCKETS"
    |> Option.value ~default:"/sockets/api1.sock,/sockets/api2.sock"
    |> String.split ~on:','
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> Array.of_list
  in
  if Array.is_empty upstreams then failwith "UPSTREAM_SOCKETS is empty";
  let listen_fd = listen_fd port in
  let epoll =
    match Linux_ext.Epoll.create with
    | Error error -> Error.raise error
    | Ok create -> create ~num_file_descrs:65_536 ~max_ready_events:1024
  in
  let endpoints = Hashtbl.create (module Int) in
  let next_upstream = ref 0 in
  Linux_ext.Epoll.set epoll listen_fd Linux_ext.Epoll.Flags.in_;
  printf "rinha_lb listening on :%d -> %s\n%!" port (String.concat_array ~sep:"," upstreams);
  while true do
    ignore (Linux_ext.Epoll.wait epoll ~timeout:`Never : [ `Ok | `Timeout ]);
    Linux_ext.Epoll.iter_ready epoll ~f:(fun fd flags ->
      if fd_to_int fd = fd_to_int listen_fd
      then accept_loop epoll endpoints listen_fd upstreams next_upstream
      else (
        match Hashtbl.find endpoints (fd_to_int fd) with
        | None -> ()
        | Some endpoint ->
          if Linux_ext.Epoll.Flags.do_intersect flags Linux_ext.Epoll.Flags.in_
          then read_into epoll endpoints endpoint;
          if Hashtbl.mem endpoints (fd_to_int fd)
             && Linux_ext.Epoll.Flags.do_intersect flags Linux_ext.Epoll.Flags.out
          then write_from epoll endpoints endpoint));
    Linux_ext.Epoll.Expert.clear_ready epoll
  done
