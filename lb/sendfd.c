#include <errno.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <unistd.h>
#include <stddef.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

/* Create an unbound DGRAM Unix socket for sending fds (reused per worker). */
CAMLprim value caml_create_send_fd_socket(value unit)
{
    CAMLparam1(unit);
    int s = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (s < 0) caml_unix_error(errno, "socket", Nothing);
    CAMLreturn(Val_int(s));
}

/*
 * caml_send_fd(send_sock, dest_path, fd_to_send)
 * Sends fd_to_send as SCM_RIGHTS ancillary data to the DGRAM socket
 * bound at dest_path.  The one-byte dummy payload is required by POSIX.
 */
CAMLprim value caml_send_fd(value send_sock_v, value dest_path_v, value fd_to_send_v)
{
    CAMLparam3(send_sock_v, dest_path_v, fd_to_send_v);
    int send_sock  = Int_val(send_sock_v);
    int fd_to_send = Int_val(fd_to_send_v);
    const char *dest_path = String_val(dest_path_v);

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    size_t plen = strlen(dest_path);
    memcpy(addr.sun_path, dest_path, plen + 1);
    socklen_t addrlen = (socklen_t)(offsetof(struct sockaddr_un, sun_path) + plen + 1);

    char cmsg_buf[CMSG_SPACE(sizeof(int))];
    char dummy = 0;
    struct iovec iov = { .iov_base = &dummy, .iov_len = 1 };
    struct msghdr msg;
    memset(&msg, 0, sizeof(msg));
    msg.msg_name    = &addr;
    msg.msg_namelen = addrlen;
    msg.msg_iov     = &iov;
    msg.msg_iovlen  = 1;
    msg.msg_control    = cmsg_buf;
    msg.msg_controllen = sizeof(cmsg_buf);

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type  = SCM_RIGHTS;
    cmsg->cmsg_len   = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(cmsg), &fd_to_send, sizeof(int));

    if (sendmsg(send_sock, &msg, 0) < 0)
        caml_unix_error(errno, "sendmsg", Nothing);

    CAMLreturn(Val_unit);
}
