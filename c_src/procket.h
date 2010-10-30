/*
 * Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>

#include <unistd.h>
#include <sys/types.h>

#include <netinet/in.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <net/if.h>

#include <sys/errno.h>

#include <sys/ioctl.h>


#define PROCKET_VERSION   "0.01"
#define MAXBUFLEN           4096    /* Largest message accepted on stdin */
#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX  sizeof(((struct sockaddr_un *)0)->sun_path)
#endif

#define IS_ERR(x) do { \
        if ((x) == -1) return (-1); \
} while (0)

#define IS_NULL(x) do { \
    if ((x) == NULL) \
    errx(EXIT_FAILURE, "%s", #x); \
} while (0);

#define IS_FALSE(x) do { \
    if ((x) != 0) \
    errx(EXIT_FAILURE, "%s", #x); \
} while (0);

#define IS_LTZERO(x) do { \
    if ((x) < 0) \
    errx(EXIT_FAILURE, "%s", #x); \
} while (0);

#define VERBOSE(x, ...) do { \
    if (ep->verbose >= x) { \
        (void)fprintf (stderr, __VA_ARGS__); \
    } \
} while (0)


extern char *__progname;


typedef struct {
    char *path;             /* path to pipe file */
    char *address;          /* <port> or <ipaddr:port> */
    char *ifname;           /* network interface name */
    int verbose;            /* Debug messages */

    in_addr_t ip;           /* IP Address */
    in_port_t port;         /* Port */

    int s;                  /* socket fd */
    int family;             /* socket family: PF_INET */
    int type;               /* socket type: SOCK_STREAM */
    int protocol;           /* socket protocol: IPPROTO_TCP */
    int backlog;            /* Listen backlog */
} PROCKET_STATE;


