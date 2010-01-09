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

int procket_pipe(PROCKET_STATE *ps);

int procket_open_tcp(void *state);
int procket_open_udp(void *state);
int procket_open_icmp(void *state);
int procket_open_raw(void *state);

int procket_open_socket(void *state, int domain, int type, int protocol);
void usage(PROCKET_STATE *ep);


    int
main(int argc, char *argv[])
{
    PROCKET_STATE *ps = NULL;
    int ch = 0;
    char *p = NULL;


    IS_NULL(ps = (PROCKET_STATE *)calloc(1, sizeof(PROCKET_STATE)));

    ps->open = &procket_open_tcp;
    ps->ipaddr = INADDR_ANY;

    while ( (ch = getopt(argc, argv, "hp:P:v")) != -1) {
        switch (ch) {
            case 'p':   /* path to pipe */
                IS_NULL(ps->path = strdup(optarg));
                if (strlen(ps->path) >= UNIX_PATH_MAX)
                    usage(ps);
                break;
            case 'P':   /* type of socket */
                switch (atoi(optarg)) {
                    case IPPROTO_ICMP:
                        ps->open = &procket_open_icmp;
                        break;
                    case IPPROTO_IP:    /* raw socket */
                        ps->open = &procket_open_raw;
                        break;
                    case IPPROTO_TCP:
                        ps->open = &procket_open_tcp;
                        break;
                    case IPPROTO_UDP:
                        ps->open = &procket_open_udp;
                        break;
                    default:
                        usage(ps);
                        break;
                }
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

    if ( (argc == 0) || (ps->path == NULL))
        usage(ps);

    IS_NULL(ps->bind = strdup(argv[0]));

    if ( (p = strchr(ps->bind, ':')) == NULL) {
        ps->port = (in_port_t)atoi(ps->bind);
    }
    else {
        struct in_addr in;

        *p++ = '\0';
        ps->port = (in_port_t)atoi(p);
        IS_LTZERO(inet_aton(ps->bind, &in));
        ps->ipaddr = in.s_addr;
    }

    IS_LTZERO(ps->open(ps));

    if (setgid(getgid()) == -1)
        err(EXIT_FAILURE, "setgid");
    if (setuid(getuid()) == -1)
        err(EXIT_FAILURE, "setuid");

    IS_LTZERO(procket_pipe(ps));

    exit (0);
}


    int
procket_open_tcp(void *state)
{
    PROCKET_STATE *ps = (PROCKET_STATE *)state;
    int r = 0;


    if ( (r = procket_open_socket(state, PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
        return (-1);

    if (listen(ps->s, BACKLOG) < 0)
        err(EXIT_FAILURE, "listen");

    return (0);
}


    int
procket_open_udp(void *state)
{
    return procket_open_socket(state, PF_INET, SOCK_DGRAM, IPPROTO_UDP);
}


    int
procket_open_icmp(void *state)
{
    return procket_open_socket(state, PF_INET, SOCK_RAW, IPPROTO_ICMP);
}


    int
procket_open_raw(void *state)
{
    PROCKET_STATE *ps = (PROCKET_STATE *)state;
    const int on = 1;

    if (procket_open_socket(state, PF_INET, SOCK_RAW, IPPROTO_TCP) < 0)
        return (-1);

    if (setsockopt(ps->s, IPPROTO_IP, IP_HDRINCL, &on, sizeof(on)) < 0)
        return (-1);

    return (0);
}


    int
procket_open_socket(void *state, int domain, int type, int protocol)
{
    PROCKET_STATE *ps = (PROCKET_STATE *)state;
    struct sockaddr_in sa = { 0 };


    sa.sin_family = domain;
    sa.sin_port = htons(ps->port);
    sa.sin_addr.s_addr = ps->ipaddr;

    if ( (ps->s = socket(domain, type, protocol)) < 0)
        return (-1);

    IS_ERR(bind(ps->s, (struct sockaddr *)&sa, sizeof(sa)));

    return (0);
}


    int
procket_pipe(PROCKET_STATE *ps)
{
    struct sockaddr_un sa = { 0 };
    int s = -1;


    (void)memcpy(sa.sun_path, ps->path, sizeof(sa.sun_path)-1);

    sa.sun_family = AF_UNIX;

    IS_LTZERO(s = socket(PF_LOCAL, SOCK_STREAM, 0));
    if (connect(s, (struct sockaddr *)&sa, sizeof(sa)) < 0)
        err(EXIT_FAILURE, "connect");

    if (ancil_send_fd(s, ps->s) < 0)
        err(EXIT_FAILURE, "ancil_send_fd");
    close (s);

    return (0);
}


    void
usage(PROCKET_STATE *ps)
{
    (void)fprintf(stderr, "%s, %s\n", __progname, PROCKET_VERSION);
    (void)fprintf(stderr,
            "usage: %s <options> <port|ipaddress:port>\n"
            "              -p <pipe>        path to pipe file\n"
            "              -P <protocol>    protocol number [tcp:6,udp:17]\n"
            "              -v               verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}


