/* Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
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

int procket_open_fd(PROCKET_STATE *ps);
int procket_check_devname(char *dev, size_t len);
int procket_parse_address(PROCKET_STATE *ps);
int procket_pipe(PROCKET_STATE *ps);
int procket_open_socket(PROCKET_STATE *ps);
int procket_open_dev(PROCKET_STATE *ps);
int procket_open_char_dev(char *dev);
void usage(PROCKET_STATE *ep);

enum {
    PROCKET_FD_SOCKET,
    PROCKET_FD_CHARDEV
};


    int
main(int argc, char *argv[])
{
    PROCKET_STATE *ps = NULL;
    int ch = 0;


    IS_NULL(ps = calloc(1, sizeof(PROCKET_STATE)));

    ps->ip = INADDR_ANY;
    ps->backlog = BACKLOG;

    ps->family = PF_INET;
    ps->type = SOCK_STREAM;
    ps->protocol = IPPROTO_TCP;

    ps->fdtype = PROCKET_FD_SOCKET;

    while ( (ch = getopt(argc, argv, "b:d:F:hp:P:T:vI:")) != -1) {
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
            case 'I': /* Interface name */
                IS_NULL(ps->ifname = strdup(optarg));
                break;
            case 'd': { /* Open a character device */
                IS_NULL(ps->dev = strdup(optarg));

                if (procket_check_devname(ps->dev, 32) < 0)
                    usage(ps);

                ps->fdtype = PROCKET_FD_CHARDEV;
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

    if (ps->path == NULL)
        usage(ps);

    if (argc > 0)
        IS_NULL(ps->address = strdup(argv[0]));

    if (procket_open_fd(ps) < 0) {
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


    int
procket_open_fd(PROCKET_STATE *ps)
{
    switch (ps->fdtype) {
        case PROCKET_FD_SOCKET:
            if (ps->protocol == IPPROTO_TCP || ps->protocol == IPPROTO_UDP) {
                if (procket_parse_address(ps) < 0)
                    return -1;
            }

            if (procket_open_socket(ps) != 0)
                return -1;
            break;

        case PROCKET_FD_CHARDEV:
            if (procket_open_dev(ps) < 0)
                return -1;
            break;
    }

    return 0;
}


    int
procket_check_devname(char *dev, size_t len)
{
    char *p = NULL;

    if (strlen(dev) >= len)
        return -1;

    for (p = dev; *p; p++) {
        if (!isascii(*p) && !isalnum(*p) && *p != '/')
            return -1;
    }

    return 0;
}


    int
procket_parse_address(PROCKET_STATE *ps)
{
    struct in_addr in;
    char *p = NULL;


    if (ps->address == NULL) {
        errno = EINVAL;
        return -1;
    }

    if ( (p = strchr(ps->address, ':')) == NULL) {
        ps->port = (in_port_t)atoi(ps->address);
        return 0;
    }

    *p++ = '\0';
    ps->port = (in_port_t)atoi(p);
    if (inet_aton(ps->address, &in) < 0)
        return -1;
    ps->ip = in.s_addr;

    return 0;
}


    int
procket_open_socket(PROCKET_STATE *ps)
{
    struct sockaddr_in sa = { 0 };
    int flags = 0;


    if ( (ps->s = socket(ps->family, ps->type, ps->protocol)) < 0)
            return -1;

#ifdef SO_BINDTODEVICE
    if(ps->ifname) {
        struct ifreq ifr;

        (void)snprintf(ifr.ifr_name, IFNAMSIZ, "%s", ps->ifname);
        if (setsockopt(ps->s, SOL_SOCKET, SO_BINDTODEVICE, &ifr, sizeof(ifr)) < 0)
            return -1;
    }
#endif

    flags = fcntl(ps->s, F_GETFL, 0);

    if (flags < 0)
        return -1;

    if (fcntl(ps->s, F_SETFL, flags|O_NONBLOCK) < 0)
        return -1;

    /* Erlang assumes the socket has already been bound */
    if ( (ps->protocol == IPPROTO_TCP) || (ps->protocol == IPPROTO_UDP)) {
        sa.sin_family = ps->family;
        sa.sin_port = htons(ps->port);
        sa.sin_addr.s_addr = ps->ip;

        if (bind(ps->s, (struct sockaddr *)&sa, sizeof(sa)) < 0)
            return -1;
    }

    return (0);
}


    int
procket_pipe(PROCKET_STATE *ps)
{
    struct sockaddr_un sa = {0};
    int s = -1;


    (void)memcpy(sa.sun_path, ps->path, sizeof(sa.sun_path)-1);

    sa.sun_family = PF_LOCAL;

    if ( (s = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0)
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
    int
procket_open_dev(PROCKET_STATE *ps)
{
    char dev[MAXPATHLEN];
    int i = 0;


    if (dev == NULL) {
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

    int
procket_open_char_dev(char *dev)
{
    int fd = -1;
    struct stat buf = {0};
    int err = 0;


    if ( (fd = open(dev, O_RDWR|O_NONBLOCK)) < 0)
        return -1;

    /* Test the file is a character device */
    if (fstat(fd, &buf) < 0) {
        err = errno;
        goto ERR;
    }

    if (!S_ISCHR(buf.st_mode)) {
        err = ENOENT;
        goto ERR;
    }

    return fd;

ERR:
    if (fd > -1)
        (void)close(fd);
    errno = err;

    return -1;
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
#ifdef SO_BINDTODEVICE
            "              -I <name>        interface [default: ANY]\n"
#endif
            "              -d <name>        open device\n"
            "              -v               verbose mode\n",
            __progname
            );

    exit (EXIT_FAILURE);
}
