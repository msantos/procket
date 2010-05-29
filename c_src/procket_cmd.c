/* Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
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
#include "procket.h"
#include "ancillary.h"


#define BACKLOG     50  /* default backlog for TCP connections */

void procket_parse_address(PROCKET_STATE *ps);
int procket_pipe(PROCKET_STATE *ps);
int procket_open_socket(PROCKET_STATE *ps);
void usage(PROCKET_STATE *ep);


    int
main(int argc, char *argv[])
{
    PROCKET_STATE *ps = NULL;
    int ch = 0;


    IS_NULL(ps = (PROCKET_STATE *)calloc(1, sizeof(PROCKET_STATE)));

    ps->ip = INADDR_ANY;
    ps->backlog = BACKLOG;

    ps->family = PF_INET;
    ps->type = SOCK_STREAM;
    ps->protocol = IPPROTO_TCP;

    while ( (ch = getopt(argc, argv, "b:F:hp:P:T:v")) != -1) {
        switch (ch) {
            case 'b':   /* listen backlog */
                ps->backlog = atoi(optarg);
                break;
            case 'F':   /* socket family/domain */
                ps->family = atoi(optarg);
                break;
            case 'p':   /* path to pipe */
                IS_NULL(ps->path = strdup(optarg));
                if (strlen(ps->path) >= UNIX_PATH_MAX)
                    usage(ps);
                break;
            case 'P':   /* socket protocol */
                ps->protocol = atoi(optarg);
                break;
            case 'T':   /* socket type */
                ps->type = atoi(optarg);
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
        usage(ps);

    if (ps->protocol == IPPROTO_TCP || ps->protocol == IPPROTO_UDP) {
        if (argc == 0)
            usage(ps);
        IS_NULL(ps->address = strdup(argv[0]));
        procket_parse_address(ps);
    }

    if (procket_open_socket(ps) != 0) {
        (void)fprintf(stderr, "%s", strerror(errno));
        exit (-errno);
    }

    if (setgid(getgid()) == -1)
        err(EXIT_FAILURE, "setgid");
    if (setuid(getuid()) == -1)
        err(EXIT_FAILURE, "setuid");

    IS_LTZERO(procket_pipe(ps));

    exit (0);
}


    void
procket_parse_address(PROCKET_STATE *ps)
{
    struct in_addr in;
    char *p = NULL;


    if ( (p = strchr(ps->address, ':')) == NULL) {
        ps->port = (in_port_t)atoi(ps->address);
        return;
    }

    *p++ = '\0';
    ps->port = (in_port_t)atoi(p);
    IS_LTZERO(inet_aton(ps->address, &in));
    ps->ip = in.s_addr;
}


    int
procket_open_socket(PROCKET_STATE *ps)
{
    struct sockaddr_in sa = { 0 };
    int flags = 0;


    if ( (ps->s = socket(ps->family, ps->type, ps->protocol)) < 0)
        return (-1);

    flags = fcntl(ps->s, F_GETFL, 0);
    flags |= O_NONBLOCK;
    (void)fcntl(ps->s, F_SETFL, flags);

    /* Erlang assumes the socket has already been bound */
    if ( (ps->protocol == IPPROTO_TCP) || (ps->protocol == IPPROTO_UDP)) {
        sa.sin_family = ps->family;
        sa.sin_port = htons(ps->port);
        sa.sin_addr.s_addr = ps->ip;

        IS_ERR(bind(ps->s, (struct sockaddr *)&sa, sizeof(sa)));
    }

    return (0);
}


    int
procket_pipe(PROCKET_STATE *ps)
{
    struct sockaddr_un sa = { 0 };
    int s = -1;


    (void)memcpy(sa.sun_path, ps->path, sizeof(sa.sun_path)-1);

    sa.sun_family = PF_LOCAL;

    IS_LTZERO(s = socket(PF_LOCAL, SOCK_STREAM, 0));
    if (connect(s, (struct sockaddr *)&sa, sizeof(sa)) < 0)
        err(EXIT_FAILURE, "connect");

    if (ancil_send_fd(s, ps->s) < 0)
        err(EXIT_FAILURE, "ancil_send_fd");

    (void)close (s);

    return (0);
}


    void
usage(PROCKET_STATE *ps)
{
    (void)fprintf(stderr, "%s, %s\n", __progname, PROCKET_VERSION);
    (void)fprintf(stderr,
            "usage: %s <options> <port|ipaddress:port>\n"
            "              -p <path>        path to Unix socket\n"
            "              -F <family>      family [default: PF_INET]\n"
            "              -P <protocol>    protocol [default: IPPROTO_TCP]\n"
            "              -T <type>        type [default: SOCK_STREAM]\n"
            "              -v               verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}


