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

#include "ancillary.h"
#include "procket.h"

#define BACKLOG 128 /* default backlog for TCP connections */

int procket_open_ns(PROCKET_STATE *ps);
int procket_open_fd(PROCKET_STATE *ps);
int procket_check_devname(char *dev, size_t len);
int procket_pipe(PROCKET_STATE *ps);
int procket_open_socket(PROCKET_STATE *ps);
int procket_create_socket(PROCKET_STATE *ps);
int procket_lookup_socket(PROCKET_STATE *ps);
int procket_open_dev(PROCKET_STATE *ps);
int procket_open_char_dev(char *dev);
void error_result(PROCKET_STATE *ps, int err);
void usage(PROCKET_STATE *ep);

enum { PROCKET_FD_SOCKET, PROCKET_FD_CHARDEV };

int main(int argc, char *argv[]) {
  PROCKET_STATE *ps = NULL;
  int ch = 0;

  ps = calloc(1, sizeof(PROCKET_STATE));

  if (ps == NULL)
    error_result(ps, errno);

  ps->backlog = BACKLOG;

  ps->family = PF_UNSPEC; /* bind IPv4 and IPv6 socket */
  ps->type = SOCK_STREAM;
  ps->protocol = IPPROTO_TCP;

  ps->fdtype = PROCKET_FD_SOCKET;

  while ((ch = getopt(argc, argv, "b:d:F:hI:N:p:P:T:u:v")) != -1) {
    switch (ch) {
    case 'b': /* listen backlog */
      ps->backlog = atoi(optarg);
      break;
    case 'F': /* socket family/domain */
      ps->family = atoi(optarg);
      break;
    case 'u': /* path to Unix socket */
      ps->path = strdup(optarg);

      if (ps->path == NULL)
        error_result(ps, errno);

      ps->pathlen = strlen(ps->path);

      if (ps->pathlen >= UNIX_PATH_MAX)
        error_result(ps, ENAMETOOLONG);
      break;
    case 'p': /* port */
      ps->port = strdup(optarg);

      if (ps->port == NULL)
        error_result(ps, errno);

      if (strlen(ps->port) >= NI_MAXSERV)
        error_result(ps, ENAMETOOLONG);
      break;
    case 'P': /* socket protocol */
      ps->protocol = atoi(optarg);
      break;
    case 'T': /* socket type */
      ps->type = atoi(optarg);
      break;
    case 'I': /* Interface name */
      ps->ifname = strdup(optarg);

      if (ps->ifname == NULL)
        error_result(ps, errno);

      if (strlen(ps->ifname) >= IFNAMSIZ)
        error_result(ps, ENAMETOOLONG);
      break;
    case 'd': /* Open a character device */
      ps->dev = strdup(optarg);

      if (ps->dev == NULL)
        error_result(ps, errno);

      if (procket_check_devname(ps->dev, 32) < 0)
        usage(ps);

      ps->fdtype = PROCKET_FD_CHARDEV;
      break;
    case 'N': /* namespace */
      ps->ns = strdup(optarg);

      if (ps->ns == NULL)
        error_result(ps, errno);
      break;
    case 'v':
      ps->verbose++;
      break;
    case 'h':
    default:
      usage(ps);
    }
  }

  argc -= optind;
  argv += optind;

  if (ps->path == NULL)
    error_result(ps, ENOENT);

  if (argc > 0) {
    ps->address = strdup(argv[0]);

    if (ps->address == NULL)
      error_result(ps, errno);

    if (strlen(ps->address) >= NI_MAXHOST)
      error_result(ps, ENAMETOOLONG);
  }

  if (procket_open_ns(ps) < 0)
    error_result(ps, errno);

  if (procket_open_fd(ps) < 0)
    error_result(ps, errno);

  if ((setgid(getgid()) < 0) || (setuid(getuid()) < 0))
    error_result(ps, errno);

  if (procket_pipe(ps) < 0)
    error_result(ps, errno);

  exit(0);
}

int procket_open_ns(PROCKET_STATE *ps) {
#ifdef HAVE_SETNS
  int fd = 0;

  if (ps->ns == NULL)
    return 0;

  fd = open(ps->ns, O_RDONLY);

  if (fd < 0)
    return -1;

  if (setns(fd, 0 /* join all namespaces */) < 0)
    return -1;
#endif

  return 0;
}

int procket_open_fd(PROCKET_STATE *ps) {
  switch (ps->fdtype) {
  case PROCKET_FD_SOCKET:
    return procket_open_socket(ps);

  case PROCKET_FD_CHARDEV:
    return procket_open_dev(ps);

  default:
    return -1;
  }
}

int procket_check_devname(char *dev, size_t len) {
  if (strlen(dev) >= len)
    return -1;

  return 0;
}

int procket_open_socket(PROCKET_STATE *ps) {
  switch (ps->protocol) {
  case IPPROTO_ICMP:
    /* getaddrinfo(3): node for SOCK_RAW must not be NULL */
    if (ps->address == NULL) {
      char *ipaddr = ps->family == AF_INET6 ? "::" : "0.0.0.0";
      ps->address = strdup(ipaddr);
      if (ps->address == NULL)
        return -1;
    }

    /* getaddrinfo(3): port for SOCK_RAW must NULL */
    free(ps->port);
    ps->port = NULL;

    /* fall through */

  case IPPROTO_TCP:
  case IPPROTO_UDP:
    if (procket_lookup_socket(ps) < 0)
      return -1;
    break;

  default:
    if (procket_create_socket(ps) < 0)
      return -1;
    break;
  }

  return 0;
}

int procket_lookup_socket(PROCKET_STATE *ps) {
  struct addrinfo hints = {0};
  struct addrinfo *res = NULL;
  struct addrinfo *rp = NULL;
  int err = 0;

  hints.ai_family = ps->family;
  hints.ai_socktype = ps->type;
  hints.ai_protocol = ps->protocol;
  hints.ai_flags = AI_NUMERICHOST | AI_NUMERICSERV | AI_PASSIVE;

  err = getaddrinfo(ps->address, ps->port, &hints, &res);

  switch (err) {
  case 0:
    break;
  case EAI_SYSTEM:
    return -1;

    /* fake errno values */
#ifdef EAI_ADDRFAMILY
  case EAI_ADDRFAMILY:
    errno = EAFNOSUPPORT;
    return -1;
#endif
  case EAI_AGAIN:
    errno = EAGAIN;
    return -1;
  case EAI_FAMILY:
    errno = EPFNOSUPPORT;
    return -1;
  case EAI_MEMORY:
    errno = EAI_MEMORY;
    return -1;
  case EAI_SERVICE:
  case EAI_SOCKTYPE:
    errno = ESOCKTNOSUPPORT;
    return -1;
  default:
    errno = EINVAL;
    return -1;
  }

  for (rp = res; rp != NULL; rp = rp->ai_next) {
    ps->family = rp->ai_family;
    ps->type = rp->ai_socktype;
    ps->protocol = rp->ai_protocol;

    if (procket_create_socket(ps) == 0) {
      if (bind(ps->s, rp->ai_addr, rp->ai_addrlen) < 0)
        return -1;
      break;
    }
  }

  freeaddrinfo(res);

  if (ps->s < 0)
    return -1;

  return 0;
}

int procket_create_socket(PROCKET_STATE *ps) {
  int flags = 0;

  ps->s = socket(ps->family, ps->type, ps->protocol);

  if (ps->s < 0)
    return -1;

#ifdef SO_BINDTODEVICE
  if (ps->ifname) {
    if (setsockopt(ps->s, SOL_SOCKET, SO_BINDTODEVICE, ps->ifname,
                   strlen(ps->ifname)) < 0)
      return -1;
  }
#endif

  flags = fcntl(ps->s, F_GETFL, 0);

  if (flags < 0)
    goto ERR;

  if (fcntl(ps->s, F_SETFL, flags | O_NONBLOCK) < 0)
    goto ERR;

  return 0;

ERR:
  if (ps->s > -1) {
    int err = errno;
    (void)close(ps->s);
    errno = err;
    ps->s = -1;
  }

  return -1;
}

int procket_pipe(PROCKET_STATE *ps) {
  struct sockaddr_un sa = {0};
  int s = -1;

  (void)memcpy(sa.sun_path, ps->path, MIN(ps->pathlen, UNIX_PATH_MAX - 1));

  sa.sun_family = PF_LOCAL;

  if ((s = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0)
    return -1;

  if (connect(s, (struct sockaddr *)&sa, sizeof(sa)) < 0)
    return -1;

  if (ancil_send_fd(s, ps->s) < 0)
    return -1;

  if (close(s) < 0)
    return -1;

  return 0;
}

/* character device support */
int procket_open_dev(PROCKET_STATE *ps) {
  char dev[MAXPATHLEN] = {0};
  int i = 0;

  if (ps->dev == NULL) {
    errno = ENXIO;
    return -1;
  }

  (void)snprintf(dev, sizeof(dev), "/dev/%s", ps->dev);

  ps->s = procket_open_char_dev(dev);

  if (ps->s > -1)
    return 0;

  switch (errno) {
  case ENOENT:
    break;
  default:
    return -1;
  }

  for (i = 0; i < 255; i++) {
    (void)snprintf(dev, sizeof(dev), "/dev/%s%d", ps->dev, i);

    ps->s = procket_open_char_dev(dev);

    if (ps->s > -1)
      return 0;

    switch (errno) {
    case EBUSY:
      break;
    default:
      return -1;
    }
  }

  return -1;
}

int procket_open_char_dev(char *dev) {
  int fd = -1;
  struct stat buf = {0};
  int err = 0;
  int flags = 0;

  if ((fd = open(dev, O_RDWR)) < 0)
    return -1;

  flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0)
    goto ERR;

  if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0)
    goto ERR;

  /* Test the file is a character device */
  if (fstat(fd, &buf) < 0)
    goto ERR;

  if (!S_ISCHR(buf.st_mode)) {
    errno = ENOENT;
    goto ERR;
  }

  return fd;

ERR:
  if (fd > -1) {
    err = errno;
    (void)close(fd);
    errno = err;
  }

  return -1;
}

void error_result(PROCKET_STATE *ps, int err) {
  if (ps->verbose > 0)
    (void)fprintf(stderr, "%s", strerror(err));

  exit(err);
}

void usage(PROCKET_STATE *ps) {
  (void)fprintf(stderr, "%s, %s\n", __progname, PROCKET_VERSION);
  (void)fprintf(
      stderr,
      "usage: %s <options> <ipaddress>\n"
      "              -b <backlog>     listen socket backlog [default:%d]\n"
      "              -u <path>        path to Unix socket\n"
      "              -p <port>        port\n"
      "              -F <family>      family [default: PF_UNSPEC]\n"
      "              -P <protocol>    protocol [default: IPPROTO_TCP]\n"
      "              -T <type>        type [default: SOCK_STREAM]\n"
#ifdef SO_BINDTODEVICE
      "              -I <name>        interface [default: ANY]\n"
#endif
      "              -d <name>        open device\n"
      "              -v               verbose mode\n",
      __progname, BACKLOG);

  exit(-1);
}
