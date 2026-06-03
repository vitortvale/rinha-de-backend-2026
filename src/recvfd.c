#include <errno.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <stddef.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

/*
 * Create and bind a DGRAM Unix socket for receiving file descriptors.
 * All forked API workers share this socket; the kernel delivers each
 * incoming fd to exactly one waiting recvmsg() call.
 */
CAMLprim value caml_create_recv_fd_socket(value path_v)
{
    CAMLparam1(path_v);
    const char *path = String_val(path_v);

    unlink(path);

    int s = socket(AF_UNIX, SOCK_DGRAM, 0);
    if (s < 0) caml_unix_error(errno, "socket", path_v);

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    size_t plen = strlen(path);
    memcpy(addr.sun_path, path, plen + 1);
    socklen_t addrlen = (socklen_t)(offsetof(struct sockaddr_un, sun_path) + plen + 1);

    if (bind(s, (struct sockaddr *)&addr, addrlen) < 0) {
        int err = errno;
        close(s);
        caml_unix_error(err, "bind", path_v);
    }
    chmod(path, 0666);

    CAMLreturn(Val_int(s));
}

static value do_recv_fd(int sock, int flags)
{
    char cmsg_buf[CMSG_SPACE(sizeof(int))];
    char dummy;
    struct iovec iov = { .iov_base = &dummy, .iov_len = 1 };
    struct msghdr msg;
    memset(&msg, 0, sizeof(msg));
    msg.msg_iov        = &iov;
    msg.msg_iovlen     = 1;
    msg.msg_control    = cmsg_buf;
    msg.msg_controllen = sizeof(cmsg_buf);

    ssize_t n;
    do {
        n = recvmsg(sock, &msg, flags);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
            return Val_int(-1);  /* sentinel: no data, caller should retry */
        caml_unix_error(errno, "recvmsg", Nothing);
    }

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    if (cmsg == NULL || cmsg->cmsg_type != SCM_RIGHTS)
        caml_failwith("recv_fd: no file descriptor in message");

    int received_fd;
    memcpy(&received_fd, CMSG_DATA(cmsg), sizeof(int));
    return Val_int(received_fd);
}

/*
 * Blocking receive — waits until an fd arrives via SCM_RIGHTS.
 * Returns the received Unix.file_descr.
 */
CAMLprim value caml_recv_fd(value sock_v)
{
    CAMLparam1(sock_v);
    CAMLreturn(do_recv_fd(Int_val(sock_v), 0));
}

/*
 * Non-blocking receive — returns -1 immediately if no datagram is ready.
 * Used after select(2) when multiple workers compete for the same socket:
 * the worker that loses the race returns -1 and loops back to select.
 */
CAMLprim value caml_recv_fd_nonblock(value sock_v)
{
    CAMLparam1(sock_v);
    CAMLreturn(do_recv_fd(Int_val(sock_v), MSG_DONTWAIT));
}
