external create_send_socket : unit -> Unix.file_descr = "caml_create_send_fd_socket"

external send_fd
  :  Unix.file_descr
  -> string
  -> Unix.file_descr
  -> unit
  = "caml_send_fd"
