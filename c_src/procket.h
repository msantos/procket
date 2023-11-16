/* Copyright (c) 2010-2023, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#ifdef __APPLE__
  #define __APPLE_USE_RFC_3542  /* For IPV6_RECVPKTINFO */
#endif

#ifdef __sun__
  #define __EXTENSIONS__ /* For IPV6_RECVPKTINFO */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>
#include <sys/param.h>

#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <net/if.h>

#include <errno.h>

#include <sys/ioctl.h>

#include <sys/stat.h>

#include <sys/uio.h>

#if defined(__sun) && defined(__SVR4)
#include <stropts.h>
#endif

#define PROCKET_VERSION   "0.04"
#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX  sizeof(((struct sockaddr_un *)0)->sun_path)
#endif

#define MAXATOMLEN  255     /* ei.h: does not include terminating NULL */

extern char *__progname;

typedef struct {
    int fdtype;             /* fd type requested */
    char *path;             /* path to pipe file */
    size_t pathlen;         /* path length (does not include trailing NULL) */
    char *address;          /* IP address */
    char *port;             /* Port */
    char *ifname;           /* network interface name */
    char *dev;              /* Open a character device */
    char *ns;               /* Open a namespace */
    int verbose;            /* Debug messages */
    int s;                  /* socket fd */
    int family;             /* socket family: PF_INET */
    int type;               /* socket type: SOCK_STREAM */
    int protocol;           /* socket protocol: IPPROTO_TCP */
    int backlog;            /* Listen backlog */
} PROCKET_STATE;
