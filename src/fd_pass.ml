external create_recv_fd_socket : string -> Unix.file_descr
  = "caml_create_recv_fd_socket"

external recv_fd : Unix.file_descr -> Unix.file_descr = "caml_recv_fd"

external recv_fd_nonblock_raw : Unix.file_descr -> int = "caml_recv_fd_nonblock"

(*
 * Non-blocking receive: returns [Some fd] if a datagram was ready,
 * or [None] if EAGAIN (another worker already consumed it).
 * Call only after select(2) says the fd_socket is readable.
 *)
let recv_fd_nonblock sock =
  let raw = recv_fd_nonblock_raw sock in
  if raw >= 0 then Some (Obj.magic raw : Unix.file_descr) else None
