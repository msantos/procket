/* Copyright (c) 2010-2023 Michael Santos <michael.santos@gmail.com>. All
 * rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifdef HAVE_SETNS
#define _GNU_SOURCE
#include <sched.h>
#endif

#include "erl_nif.h"

#include "erl_driver.h"

#include "ancillary.h"
#include "procket.h"
#include "procket_constants.h"

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum);
void procket_alloc_free(ErlNifEnv *env, void *obj);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_undefined;

static ErlNifResourceType *PROCKET_ALLOC_RESOURCE;

typedef struct _procket_alloc_state {
  size_t size;
  void *buf;
} PROCKET_ALLOC_STATE;

/* Grow or shrink a binary.
 *
 * We increase the size of the binary to indicate to the
 * caller the size required, e.g., recvfrom/2 will
 * return the number of bytes needed for the address
 * parameter if the provided address structure was too
 * small.
 *
 * One problem is that enif_realloc_binary() doesn't zero
 * allocated bytes, so we may leak internal VM data in the
 * binary.
 *
 * In the case where we are increasing the size of the binary,
 * zero the trailing bytes.
 *
 * WARNING: Only use with mutable binaries. If the bin is
 * immutable, the result may or may not be mutable.
 *
 */
#define PROCKET_REALLOC(bin, nsize)                                            \
  do {                                                                         \
    size_t osize = bin.size;                                                   \
    if (nsize != bin.size) {                                                   \
      if (!enif_realloc_binary(&bin, nsize))                                   \
        return error_tuple(env, ENOMEM);                                       \
      if (nsize > osize)                                                       \
        (void)memset(bin.data + osize, 0, bin.size - osize);                   \
    }                                                                          \
  } while (0);

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  atom_eagain = enif_make_atom(env, "eagain");
  atom_undefined = enif_make_atom(env, "undefined");

  if ((PROCKET_ALLOC_RESOURCE = enif_open_resource_type(
           env, NULL, "procket_alloc_resource", procket_alloc_free,
           ERL_NIF_RT_CREATE, NULL)) == NULL)
    return -1;

  return (0);
}

/* Stubs for reload and upgrade */
static int reload(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM info) {
  return 0;
}

static int upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data,
                   ERL_NIF_TERM info) {
  return 0;
}

/* Retrieve the file descriptor from the forked privileged process */
/* 0: connected Unix socket */
static ERL_NIF_TERM nif_fdrecv(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  int fd = -1; /* connected socket */
  int s = -1;  /* socket received from pipe */

  if (!enif_get_int(env, argv[0], &fd))
    return enif_make_badarg(env);

  if (ancil_recv_fd(fd, &s) < 0) {
    int err = errno;
    (void)close(fd);
    return error_tuple(env, err);
  }

  if (close(fd) < 0)
    return error_tuple(env, errno);

  return enif_make_tuple2(env, atom_ok, enif_make_int(env, s));
}

static ERL_NIF_TERM nif_setns(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
#ifdef HAVE_SETNS
  int fd;
  ErlNifBinary buf = {0};
  int nstype = 0;
  int errnum = 0;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &buf))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[1], &nstype))
    return enif_make_badarg(env);

  PROCKET_REALLOC(buf, buf.size + 1);
  buf.data[buf.size - 1] = '\0';

  fd =
      open((const char *)buf.data, O_RDONLY); /* Get descriptor for namespace */
  if (fd < 0)
    return error_tuple(env, errno);

  if (setns(fd, nstype) == -1) {
    errnum = errno;
    (void)close(fd);
    return error_tuple(env, errnum);
  }

  (void)close(fd);

  return atom_ok;
#else
  return error_tuple(env, ENOTSUP);
#endif
}

/*  0: protocol, 1: type, 2: family */
static ERL_NIF_TERM nif_socket(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  int s = -1;
  int family = 0;
  int type = 0;
  int protocol = 0;
  int flags = 0;

  if (!enif_get_int(env, argv[0], &family))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[1], &type))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &protocol))
    return enif_make_badarg(env);

  s = socket(family, type, protocol);
  if (s < 0)
    return error_tuple(env, errno);

  flags = fcntl(s, F_GETFL, 0);

  if (flags < 0)
    return error_tuple(env, errno);

  if (fcntl(s, F_SETFL, flags | O_NONBLOCK) < 0)
    return error_tuple(env, errno);

  return enif_make_tuple2(env, atom_ok, enif_make_int(env, s));
}

/* 0: file descriptor, 1: backlog */
static ERL_NIF_TERM nif_listen(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  int s = -1;
  int backlog = 5;

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[1], &backlog))
    return enif_make_badarg(env);

  if (listen(s, backlog) < 0)
    return error_tuple(env, errno);

  return atom_ok;
}

/* 0: socket, 1: struct sockaddr length */
static ERL_NIF_TERM nif_accept(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  int l = -1;
  int s = -1;
  unsigned long salen = 0;
  ErlNifBinary sa = {0};
  int flags = 0;

  if (!enif_get_int(env, argv[0], &l))
    return enif_make_badarg(env);

  if (!enif_get_ulong(env, argv[1], &salen))
    return enif_make_badarg(env);

  if (!enif_alloc_binary(salen, &sa))
    return error_tuple(env, ENOMEM);

  s = accept(l, (sa.size == 0 ? NULL : (struct sockaddr *)sa.data),
             (socklen_t *)&salen);
  if (s < 0)
    return error_tuple(env, errno);

  flags = fcntl(s, F_GETFL, 0);

  if (flags < 0)
    return error_tuple(env, errno);

  if (fcntl(s, F_SETFL, flags | O_NONBLOCK) < 0)
    return error_tuple(env, errno);

  PROCKET_REALLOC(sa, salen);

  return enif_make_tuple3(env, atom_ok, enif_make_int(env, s),
                          enif_make_binary(env, &sa));
}

/* 0: file descriptor */
static ERL_NIF_TERM nif_close(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  int sockfd = -1;

  if (!enif_get_int(env, argv[0], &sockfd))
    return enif_make_badarg(env);

  if (close(sockfd) < 0)
    return error_tuple(env, errno);

  return atom_ok;
}

/* 0: socket, 1: length */
/* 0: socket, 1: length, 2: flags, 3: struct sockaddr length */
static ERL_NIF_TERM nif_recvfrom(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  int sockfd = -1;
  unsigned long len = 0;
  unsigned long salen = 0;
  int flags = 0;

  ErlNifBinary buf = {0};
  ErlNifBinary sa = {0};
  ssize_t bufsz = 0;

  if (!enif_get_int(env, argv[0], &sockfd))
    return enif_make_badarg(env);
  if (!enif_get_ulong(env, argv[1], &len))
    return enif_make_badarg(env);
  if (!enif_get_int(env, argv[2], &flags))
    return enif_make_badarg(env);
  if (!enif_get_ulong(env, argv[3], &salen))
    return enif_make_badarg(env);

  if (!enif_alloc_binary(len, &buf))
    return error_tuple(env, ENOMEM);

  if (!enif_alloc_binary(salen, &sa))
    return error_tuple(env, ENOMEM);

  if ((bufsz = recvfrom(sockfd, buf.data, buf.size, flags,
                        (sa.size == 0 ? NULL : (struct sockaddr *)sa.data),
                        (socklen_t *)&salen)) == -1) {
    int err = errno;
    enif_release_binary(&buf);
    enif_release_binary(&sa);
    return error_tuple(env, err);
  }

  PROCKET_REALLOC(buf, bufsz);
  PROCKET_REALLOC(sa, salen);

  return enif_make_tuple3(env, atom_ok, enif_make_binary(env, &buf),
                          enif_make_binary(env, &sa));
}

/* 0: socket, 1: buffer, 2: flags, 3: struct sockaddr */
static ERL_NIF_TERM nif_sendto(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  int sockfd = -1;
  int flags = 0;
  ssize_t n = 0;

  ErlNifBinary buf = {0};
  ErlNifBinary sa = {0};

  if (!enif_get_int(env, argv[0], &sockfd))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &buf))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &flags))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[3], &sa))
    return enif_make_badarg(env);

  n = sendto(sockfd, buf.data, buf.size, flags,
             (sa.size == 0 ? NULL : (struct sockaddr *)sa.data), sa.size);

  if (n < 0)
    return error_tuple(env, errno);

  return enif_make_tuple2(env, atom_ok, enif_make_int64(env, n));
}

/* 0: socket, 1: length */
static ERL_NIF_TERM nif_read(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
  int fd = -1;
  unsigned long len = 0;

  ErlNifBinary buf = {0};
  ssize_t bufsz = 0;

  if (!enif_get_int(env, argv[0], &fd))
    return enif_make_badarg(env);
  if (!enif_get_ulong(env, argv[1], &len))
    return enif_make_badarg(env);

  if (!enif_alloc_binary(len, &buf))
    return error_tuple(env, ENOMEM);

  if ((bufsz = read(fd, buf.data, buf.size)) == -1) {
    int err = errno;
    enif_release_binary(&buf);
    return error_tuple(env, err);
  }

  PROCKET_REALLOC(buf, bufsz);

  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}

/* 0: fd, 1: buffer */
static ERL_NIF_TERM nif_write(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  int fd = -1;
  ssize_t n = 0;

  ErlNifBinary buf = {0};

  if (!enif_get_int(env, argv[0], &fd))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &buf))
    return enif_make_badarg(env);

  n = write(fd, buf.data, buf.size);

  if (n < 0)
    return error_tuple(env, errno);

  return enif_make_tuple2(env, atom_ok, enif_make_int64(env, n));
}

#define IOVMAX 256

/* 0: fd, 1: list of buffers */
static ERL_NIF_TERM nif_writev(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM head = {0};
  ERL_NIF_TERM tail = {0};
  struct iovec iovs[IOVMAX];
  int fd = -1;
  unsigned iovcnt;
  ssize_t n = 0;

  if (!enif_get_int(env, argv[0], &fd))
    return enif_make_badarg(env);

  tail = argv[1];

  if (!enif_get_list_length(env, tail, &iovcnt))
    return enif_make_badarg(env);

  if (!iovcnt || iovcnt > IOVMAX)
    return enif_make_badarg(env);

  iovcnt = 0;

  while (enif_get_list_cell(env, tail, &head, &tail)) {
    struct iovec *iov = &iovs[iovcnt++];
    ErlNifBinary buf = {0};

    if (!enif_inspect_binary(env, head, &buf))
      return enif_make_badarg(env);

    iov->iov_base = buf.data;
    iov->iov_len = buf.size;
  }

  n = writev(fd, iovs, iovcnt);

  if (n < 0)
    return error_tuple(env, errno);

  return enif_make_tuple2(env, atom_ok, enif_make_int64(env, n));
}

/* 0: socket descriptor, 1: struct sockaddr */
static ERL_NIF_TERM nif_bind(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
  int s = -1;
  ErlNifBinary sa = {0};

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &sa))
    return enif_make_badarg(env);

  if (bind(s, (sa.size == 0 ? NULL : (struct sockaddr *)sa.data), sa.size) < 0)
    return error_tuple(env, errno);

  return atom_ok;
}

/* 0: socket descriptor, 1: struct sockaddr */
static ERL_NIF_TERM nif_connect(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  int s = -1;
  ErlNifBinary sa = {0};

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &sa))
    return enif_make_badarg(env);

  if (connect(s, (sa.size == 0 ? NULL : (struct sockaddr *)sa.data), sa.size) <
      0)
    return error_tuple(env, errno);

  return atom_ok;
}

/* 0: (int)socket descriptor, 1: (int)device dependent request,
 * 2: (char *)argp, pointer to structure | int
 */
static ERL_NIF_TERM nif_ioctl(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  int s = -1;
  unsigned long req = 0;
  ErlNifBinary arg = {0};
  int n = 0;

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_get_ulong(env, argv[1], &req))
    return enif_make_badarg(env);

  if (enif_inspect_binary(env, argv[2], &arg)) {
    /* Make the binary mutable */
    if (!enif_realloc_binary(&arg, arg.size))
      return error_tuple(env, ENOMEM);

    if (ioctl(s, req, arg.data) < 0)
      return error_tuple(env, errno);
  } else if (enif_get_int(env, argv[2], &n)) {
    /* XXX integer args allow the caller to
     * XXX pass in arbitrary pointers */
    if (ioctl(s, req, n) < 0)
      return error_tuple(env, errno);

    /* return an empty binary */
    if (!enif_alloc_binary(0, &arg))
      return error_tuple(env, ENOMEM);
  } else
    return enif_make_badarg(env);

  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &arg));
}

/* 0: int socket descriptor, 1: int level,
 * 2: int optname, 3: void *optval
 */
static ERL_NIF_TERM nif_setsockopt(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  int s = -1;
  int level = 0;
  int optname = 0;
  ErlNifBinary optval = {0};

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[1], &level))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &optname))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[3], &optval))
    return enif_make_badarg(env);

  if (setsockopt(s, level, optname, optval.data, optval.size) < 0)
    return error_tuple(env, errno);

  return atom_ok;
}

/* 0: int socket descriptor, 1: int level,
 * 2: int optname, 3: void *optval
 */
static ERL_NIF_TERM nif_getsockopt(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  int s = -1;
  int level = 0;
  int optname = 0;
  ErlNifBinary optval = {0};
  socklen_t optlen = 0;

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[1], &level))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &optname))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[3], &optval))
    return enif_make_badarg(env);

  /* Make the binary mutable */
  if (!enif_realloc_binary(&optval, optval.size))
    return error_tuple(env, ENOMEM);

  optlen = optval.size;

  if (getsockopt(s, level, optname, (optval.size == 0 ? NULL : optval.data),
                 &optlen) < 0)
    return error_tuple(env, errno);

  PROCKET_REALLOC(optval, optlen);

  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &optval));
}

// int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
static ERL_NIF_TERM nif_getsockname(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  int s = -1;
  ErlNifBinary addr = {0};
  socklen_t addrlen = 0;

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &addr))
    return enif_make_badarg(env);

  /* Make the binary mutable */
  if (!enif_realloc_binary(&addr, addr.size))
    return error_tuple(env, ENOMEM);

  addrlen = addr.size;

  if (getsockname(s, (struct sockaddr *)addr.data, (socklen_t *)&addrlen) < 0)
    return error_tuple(env, errno);

  PROCKET_REALLOC(addr, addrlen);

  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &addr));
}

static ERL_NIF_TERM nif_recvmsg(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  int s = -1;
  ErlNifBinary buf = {0};
  ErlNifBinary src_addr = {0};
  char *ctrldata = NULL;
  ERL_NIF_TERM ctrldatalist;
  int flags = 0;
  unsigned long bufsize = 0;
  unsigned long ctrlsize = 0;
  unsigned long sasize = 0;

  struct iovec iov[1];
  struct msghdr message;
  struct cmsghdr *cmsg;

  ssize_t n = 0;

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (!enif_get_ulong(env, argv[1], &bufsize))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &flags))
    return enif_make_badarg(env);

  if (!enif_get_ulong(env, argv[3], &ctrlsize))
    return enif_make_badarg(env);

  if (!enif_get_ulong(env, argv[4], &sasize))
    return enif_make_badarg(env);

  if (!enif_alloc_binary(bufsize, &buf))
    return error_tuple(env, ENOMEM);

  if (ctrlsize > 0 && !(ctrldata = malloc(ctrlsize))) {
    enif_release_binary(&buf);
    return error_tuple(env, ENOMEM);
  }

  if (!enif_alloc_binary(sasize, &src_addr)) {
    enif_release_binary(&buf);
    free(ctrldata);
    return error_tuple(env, ENOMEM);
  }

  iov[0].iov_base = (buf.size == 0 ? NULL : buf.data);
  iov[0].iov_len = buf.size;

  message.msg_name = (src_addr.size == 0 ? NULL : src_addr.data);
  message.msg_namelen = src_addr.size;
  message.msg_iov = iov;
  message.msg_iovlen = 1;
  message.msg_control = ctrldata;
  message.msg_controllen = ctrlsize;

  n = recvmsg(s, &message, flags);

  if (n < 0) {
    int err = errno;
    enif_release_binary(&buf);
    enif_release_binary(&src_addr);
    free(ctrldata);
    return error_tuple(env, err);
  }

  /* resize the binary to the actual size of the received packet
   *
   * XXX On error, the macro will return ENOMEM here, leaking buf,
   * XXX src_addr and ctrldata. Since the VM has OOM'ed, it will probably
   * XXX crash anyway.
   */
  PROCKET_REALLOC(buf, n);
  PROCKET_REALLOC(src_addr, message.msg_namelen);

  ctrldatalist = enif_make_list(env, 0);

  for (cmsg = CMSG_FIRSTHDR(&message); cmsg != NULL;
       cmsg = CMSG_NXTHDR(&message, cmsg)) {
    size_t len = cmsg->cmsg_len - CMSG_LEN(0);
    ErlNifBinary cdata = {0};

    if (!enif_alloc_binary(len, &cdata)) {
      enif_release_binary(&buf);
      enif_release_binary(&src_addr);
      free(ctrldata);
      return error_tuple(env, ENOMEM);
    }

    memcpy(cdata.data, CMSG_DATA(cmsg), len);

    ctrldatalist = enif_make_list_cell(
        env,
        enif_make_tuple3(env, enif_make_int(env, cmsg->cmsg_level),
                         enif_make_int(env, cmsg->cmsg_type),
                         enif_make_binary(env, &cdata)),
        ctrldatalist);
  }

  free(ctrldata);

  return enif_make_tuple5(
      env, atom_ok, enif_make_binary(env, &buf), /* Data packet */
      enif_make_int(env, message.msg_flags), /* the message flags, eg. MSG_EOR,
                                                MSG_OOB, etc. */
      ctrldatalist, /* array of 3-tuples of {cmsg->cmsg_level, cmsg->cmsg_type,
                       cmsg->cmsg_data} */
      enif_make_binary(env,
                       &src_addr) /* source address, as a sockaddr_storage */
  );
}

static ERL_NIF_TERM nif_sendmsg(ErlNifEnv *env, int argc,
                                const ERL_NIF_TERM argv[]) {
  int sockfd = -1;
  int flags = 0;

  ssize_t n = 0;

  ErlNifBinary buf = {0};
  ErlNifBinary sa = {0};

  ERL_NIF_TERM cdata_list, head, tail;

  char *cdata = NULL;
  struct iovec iov[1];
  struct msghdr message;
  struct cmsghdr *cmsg;
  size_t cdata_size = 0;

  if (!enif_get_int(env, argv[0], &sockfd))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &buf))
    return enif_make_badarg(env);

  if (!enif_get_int(env, argv[2], &flags))
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[3]))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[4], &sa))
    return enif_make_badarg(env);

  cdata_list = argv[3];

  // figure out how much control data we'll need to send
  while (enif_get_list_cell(env, cdata_list, &head, &tail)) {
    const ERL_NIF_TERM *fields;
    int arity;
    int level, type;
    ErlNifBinary cdata_field = {0};
    if (!enif_get_tuple(env, head, &arity, &fields) || arity != 3) {
      return enif_make_badarg(env);
    }

    if (!enif_get_int(env, fields[0], &level)) {
      return enif_make_badarg(env);
    }

    if (!enif_get_int(env, fields[1], &type)) {
      return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, fields[2], &cdata_field)) {
      return enif_make_badarg(env);
    }

    cdata_size += CMSG_SPACE(cdata_field.size);

    cdata_list = tail;
  }

  if (cdata_size > 0) {
    // allocate enough control data space, if any
    // freebsd throws einval if the cdata length is 0
    // but the pointer isn't NULL
    if (!(cdata = malloc(cdata_size))) {
      return error_tuple(env, ENOMEM);
    }
  }

  // set up the iov and msghdr stuff
  iov[0].iov_base = (buf.size == 0 ? NULL : buf.data);
  iov[0].iov_len = buf.size;

  message.msg_name = (sa.size == 0 ? NULL : sa.data);
  message.msg_namelen = sa.size;
  message.msg_iov = iov;
  message.msg_iovlen = 1;
  message.msg_control = cdata;
  message.msg_controllen = cdata_size;

  // loop over the control data again, this time filling in the data in the
  // msghdr
  cdata_list = argv[3];

  cmsg = CMSG_FIRSTHDR(&message);

  while (cmsg && enif_get_list_cell(env, cdata_list, &head, &tail)) {
    const ERL_NIF_TERM *fields;
    int arity;
    unsigned char *cmsg_data;
    ErlNifBinary cdata_field = {0};
    // don't need to check here, we'd have crashed in the last loop if
    // things were wrong
    enif_get_tuple(env, head, &arity, &fields);
    enif_get_int(env, fields[0], &cmsg->cmsg_level);
    enif_get_int(env, fields[1], &cmsg->cmsg_type);
    enif_inspect_binary(env, fields[2], &cdata_field);
    cmsg_data = CMSG_DATA(cmsg);
    // copy the control data into the cdata struct
    memcpy(cmsg_data, cdata_field.data, cdata_field.size);

    // set the length
    cmsg->cmsg_len = CMSG_LEN(cdata_field.size);

    cdata_list = tail;
    cmsg = CMSG_NXTHDR(&message, cmsg);
  }

  n = sendmsg(sockfd, &message, flags);

  if (n < 0)
    return error_tuple(env, errno);

  return enif_make_tuple2(env, atom_ok, enif_make_int64(env, n));
}

/* Allocate structures for ioctl
 *
 * Some ioctl request structures have a field pointing
 * to a user allocated buffer.
 */

/* 0: list */
static ERL_NIF_TERM nif_alloc(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM head = {0};
  ERL_NIF_TERM tail = {0};

  int arity = 0;
  char key[MAXATOMLEN + 1]; /* Includes terminating NULL */
  const ERL_NIF_TERM *array = NULL;

  ERL_NIF_TERM resources = {0};
  ErlNifBinary req = {0};

  if (!enif_is_list(env, argv[0]) || enif_is_empty_list(env, argv[0]))
    return enif_make_badarg(env);

  resources = enif_make_list(env, 0);
  if (!enif_alloc_binary(0, &req))
    return error_tuple(env, ENOMEM);

  tail = argv[0];

  /* [binary(), {ptr, integer()}, {ptr, binary()}, ...] */
  while (enif_get_list_cell(env, tail, &head, &tail)) {
    int index = req.size;
    ErlNifBinary bin = {0};

    if (enif_inspect_binary(env, head, &bin)) {
      enif_realloc_binary(&req, req.size + bin.size);
      (void)memcpy(req.data + index, bin.data, bin.size);
    } else if (enif_get_tuple(env, head, &arity, &array)) {
      PROCKET_ALLOC_STATE *p = NULL;
      ERL_NIF_TERM res = {0};
      size_t val = 0;
      ErlNifBinary initial = {0};

      if ((arity != 2) ||
          !enif_get_atom(env, array[0], key, sizeof(key), ERL_NIF_LATIN1) ||
          (strcmp(key, "ptr") != 0))
        return enif_make_badarg(env);

      if (!(enif_get_ulong(env, array[1], (unsigned long *)&val) && val > 0) &&
          !(enif_inspect_binary(env, array[1], &initial) && initial.size > 0))
        return enif_make_badarg(env);

      val = (initial.size > 0) ? initial.size : val;

      p = enif_alloc_resource(PROCKET_ALLOC_RESOURCE,
                              sizeof(PROCKET_ALLOC_STATE));

      if (p == NULL)
        return error_tuple(env, ENOMEM);

      p->size = val;
      p->buf = calloc(val, 1);

      if (p->buf == NULL) {
        enif_release_resource(p);
        return error_tuple(env, ENOMEM);
      }

      if (initial.size > 0)
        (void)memcpy(p->buf, initial.data, p->size);

      if (!enif_realloc_binary(&req, req.size + sizeof(void *)))
        return error_tuple(env, ENOMEM);

      (void)memcpy(req.data + index, &p->buf, sizeof(void *));

      res = enif_make_resource(env, p);
      enif_release_resource(p);

      resources = enif_make_list_cell(env, res, resources);
    } else
      return enif_make_badarg(env);
  }

  return enif_make_tuple3(env, atom_ok, enif_make_binary(env, &req), resources);
}

/* 0: resource */
static ERL_NIF_TERM nif_buf(ErlNifEnv *env, int argc,
                            const ERL_NIF_TERM argv[]) {
  PROCKET_ALLOC_STATE *p = NULL;

  ErlNifBinary buf = {0};

  if (!enif_get_resource(env, argv[0], PROCKET_ALLOC_RESOURCE, (void **)&p))
    return enif_make_badarg(env);

  if (!enif_alloc_binary(p->size, &buf))
    return error_tuple(env, ENOMEM);

  (void)memcpy(buf.data, p->buf, buf.size);

  return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}

/* 0: resource, 1: binary */
static ERL_NIF_TERM nif_memcpy(ErlNifEnv *env, int argc,
                               const ERL_NIF_TERM argv[]) {
  PROCKET_ALLOC_STATE *p = NULL;
  ErlNifBinary buf = {0};

  if (!enif_get_resource(env, argv[0], PROCKET_ALLOC_RESOURCE, (void **)&p))
    return enif_make_badarg(env);

  if (!enif_inspect_binary(env, argv[1], &buf) || buf.size > p->size)
    return enif_make_badarg(env);

  (void)memcpy(p->buf, buf.data, buf.size);

  return atom_ok;
}

static ERL_NIF_TERM nif_socket_levels(ErlNifEnv *env, int argc,
                                      const ERL_NIF_TERM argv[]) {
  const struct procket_define *p = NULL;
  ERL_NIF_TERM list = {0};

  list = enif_make_list(env, 0);

  for (p = procket_socket_level; p->key != NULL; p++) {
    list =
        enif_make_list_cell(env,
                            enif_make_tuple2(env, enif_make_atom(env, p->key),
                                             enif_make_uint(env, p->val)),
                            list);
  }

  return list;
}

static ERL_NIF_TERM nif_socket_level(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  char buf[256] = {0};
  const struct procket_define *p = NULL;

  if (!enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1))
    return enif_make_badarg(env);

  for (p = procket_socket_level; p->key != NULL; p++) {
    if (!strcmp(buf, p->key))
      return enif_make_int(env, p->val);
  }

  return atom_undefined;
}

static ERL_NIF_TERM nif_socket_optnames(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  const struct procket_define *p = NULL;
  ERL_NIF_TERM list = {0};

  list = enif_make_list(env, 0);

  for (p = procket_socket_optname; p->key != NULL; p++) {
    list =
        enif_make_list_cell(env,
                            enif_make_tuple2(env, enif_make_atom(env, p->key),
                                             enif_make_uint(env, p->val)),
                            list);
  }

  return list;
}

static ERL_NIF_TERM nif_socket_optname(ErlNifEnv *env, int argc,
                                       const ERL_NIF_TERM argv[]) {
  char buf[256] = {0};
  const struct procket_define *p = NULL;

  if (!enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1))
    return enif_make_badarg(env);

  for (p = procket_socket_optname; p->key != NULL; p++) {
    if (!strcmp(buf, p->key))
      return enif_make_int(env, p->val);
  }

  return atom_undefined;
}

static ERL_NIF_TERM nif_socket_protocols(ErlNifEnv *env, int argc,
                                         const ERL_NIF_TERM argv[]) {
  const struct procket_define *p = NULL;
  ERL_NIF_TERM list = {0};

  list = enif_make_list(env, 0);

  for (p = procket_socket_protocol; p->key != NULL; p++) {
    list =
        enif_make_list_cell(env,
                            enif_make_tuple2(env, enif_make_atom(env, p->key),
                                             enif_make_uint(env, p->val)),
                            list);
  }

  return list;
}

static ERL_NIF_TERM nif_socket_protocol(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  char buf[256] = {0};
  const struct procket_define *p = NULL;

  if (!enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1))
    return enif_make_badarg(env);

  for (p = procket_socket_protocol; p->key != NULL; p++) {
    if (!strcmp(buf, p->key))
      return enif_make_int(env, p->val);
  }

  return atom_undefined;
}

/* 0: errno */
static ERL_NIF_TERM nif_errno_id(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  int err = -1;

  if (!enif_get_int(env, argv[0], &err))
    return enif_make_badarg(env);

  return enif_make_atom(env, erl_errno_id(err));
}

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum) {
  return enif_make_tuple2(env, atom_error,
                          enif_make_atom(env, erl_errno_id(errnum)));
}

void procket_alloc_free(ErlNifEnv *env, void *obj) {
  PROCKET_ALLOC_STATE *p = obj;

  if (p->buf == NULL)
    return;

  free(p->buf);
  p->buf = NULL;
  p->size = 0;
}

static ERL_NIF_TERM nif_set_sock_nonblock(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  int s = -1;
  int flags = 0;

  if (!enif_get_int(env, argv[0], &s))
    return enif_make_badarg(env);

  if (s < 0)
    return error_tuple(env, errno);

  flags = fcntl(s, F_GETFL, 0);

  if (flags < 0)
    return error_tuple(env, errno);

  if (fcntl(s, F_SETFL, flags | O_NONBLOCK) < 0)
    return error_tuple(env, errno);

  return atom_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"fdrecv", 1, nif_fdrecv},

    {"close", 1, nif_close},

    {"accept", 2, nif_accept},
    {"bind", 2, nif_bind},
    {"connect", 2, nif_connect},
    {"getsockname", 2, nif_getsockname},
    {"getsockopt_nif", 4, nif_getsockopt},
    {"listen", 2, nif_listen},
    {"read", 2, nif_read},
    {"write_nif", 2, nif_write},
    {"writev_nif", 2, nif_writev},

    {"ioctl", 3, nif_ioctl},
    {"socket_nif", 3, nif_socket},
    {"setns", 2, nif_setns},
    {"recvmsg_nif", 5, nif_recvmsg},
    {"sendmsg_nif", 5, nif_sendmsg},

    {"recvfrom", 4, nif_recvfrom},
    {"sendto_nif", 4, nif_sendto},
    {"setsockopt_nif", 4, nif_setsockopt},

    {"alloc_nif", 1, nif_alloc},
    {"buf", 1, nif_buf},
    {"memcpy", 2, nif_memcpy},

    {"socket_level", 0, nif_socket_levels},
    {"socket_optname", 0, nif_socket_optnames},
    {"socket_protocol", 0, nif_socket_protocols},
    {"socket_level", 1, nif_socket_level},
    {"socket_optname", 1, nif_socket_optname},
    {"socket_protocol", 1, nif_socket_protocol},

    {"errno_id", 1, nif_errno_id},

    {"set_sock_nonblock", 1, nif_set_sock_nonblock}};

ERL_NIF_INIT(procket, nif_funcs, load, reload, upgrade, NULL)
