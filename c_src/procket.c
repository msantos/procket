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
#include "erl_nif.h"
#include "ancillary.h"
#include "procket.h"

#define BACKLOG     5

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *err);
static ERL_NIF_TERM error_message(ErlNifEnv *env, char *err, char *msg);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_nodata;


    static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_nodata = enif_make_atom(env, "nodata");

    return (0);
}


    static ERL_NIF_TERM
nif_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sock_fd = -1;
    struct sockaddr_un sa = { 0 };
    int flags = 0;


    if (enif_get_string(env, argv[0], sa.sun_path, sizeof(sa.sun_path), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    sa.sun_family = PF_LOCAL;

    sock_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (sock_fd < 0)
        return error_message(env, "socket", strerror(errno));

    flags = fcntl(sock_fd, F_GETFL, 0);
    flags |= O_NONBLOCK;
    (void)fcntl(sock_fd, F_SETFL, flags);

    if (bind(sock_fd, (struct sockaddr *)&sa, sizeof(sa)) < 0)
        return error_message(env, "bind", strerror(errno));

    if (listen(sock_fd, BACKLOG) < 0)
        return error_message(env, "listen", strerror(errno));

    return enif_make_tuple(env, 2,
           atom_ok,
           enif_make_int(env, sock_fd));
}


    static ERL_NIF_TERM
nif_poll(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sock_fd = -1;        /* listening socket */
    int fd = -1;             /* connected socket */
    int s = -1;              /* socket received from pipe */
    struct sockaddr_un sa = { 0 };
    socklen_t socklen = 0;


    if (!enif_get_int(env, argv[0], &sock_fd))
        return enif_make_badarg(env);

    fd = accept(sock_fd, (struct sockaddr *)&sa, &socklen);
    if (fd < 0)
        return error_message(env, "accept", strerror(errno));

    if (ancil_recv_fd(fd, &s) < 0) {
        (void)close (fd);
        return error_message(env, "recvmsg", strerror(errno));
    }

    (void)close (fd);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_int(env, s));
}


/* 0: path to socket
 * 1: file descriptor
 */
    static ERL_NIF_TERM
nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct sockaddr_un sa = { 0 };
    int sockfd = -1;


    if (enif_get_string(env, argv[0], sa.sun_path, sizeof(sa.sun_path), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &sockfd))
        return enif_make_badarg(env);

    if (unlink(sa.sun_path) < 0)
        return error_message(env, "unlink", strerror(errno));

    (void)close(sockfd);

    return atom_ok;
}


/* 0: socket, 1: length */
    static ERL_NIF_TERM
nif_recvfrom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sockfd = -1;
    int len = 0;
    ssize_t bufsz = 0;
    ErlNifBinary buf;

    if (!enif_get_int(env, argv[0], &sockfd))
        return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &len))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(env, len, &buf))
        return error_tuple(env, "out_of_memory");

    if ( (bufsz = recvfrom(sockfd, buf.data, buf.size, 0, NULL, NULL)) == -1) {
        switch (errno) {
            case EAGAIN:
            case EINTR:
                return atom_nodata;
            default:
                return error_tuple(env, strerror(errno));
        }
    }

    if (bufsz != buf.size)
        enif_realloc_binary(env, &buf, bufsz);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_binary(env, &buf));
}


/* 0: socket, 1: buffer, 2: flags, 3: struct sockaddr */
    static ERL_NIF_TERM
nif_sendto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sockfd = -1;
    int flags = 0;

    ErlNifBinary buf;
    ErlNifBinary sa;


    if (!enif_get_int(env, argv[0], &sockfd))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &buf))
        return enif_make_badarg(env);
    
    if (!enif_get_int(env, argv[2], &flags))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[3], &sa))
        return enif_make_badarg(env);

    if (sendto(sockfd, buf.data, buf.size, flags, (struct sockaddr *)sa.data, sa.size) == -1)
        return enif_make_tuple(env, 2,
            atom_error,
            enif_make_tuple(env, 2,
            enif_make_int(env, errno),
            enif_make_string(env, strerror(errno), ERL_NIF_LATIN1)));

    return atom_ok;
}


/* 0: socket descriptor, 1: struct sockaddr */
    static ERL_NIF_TERM
nif_bind(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int s = -1;
    ErlNifBinary sa;


    if (!enif_get_int(env, argv[0], &s))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &sa))
        return enif_make_badarg(env);

    if (bind(s, (struct sockaddr *)sa.data, sa.size) < 0)
        return error_tuple(env, strerror(errno));

    return atom_ok;
}

/* 0: (int)socket descriptor, 1: (int)device dependent request,
 * 2: (char *)argp, pointer to structure
 */
    static ERL_NIF_TERM
nif_ioctl(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int s = -1;
    int req = 0;
    ErlNifBinary ifr;


    if (!enif_get_int(env, argv[0], &s))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &req))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[2], &ifr))
        return enif_make_badarg(env);

    if (!enif_realloc_binary(env, &ifr, ifr.size))
        return enif_make_badarg(env);

    if (ioctl(s, req, ifr.data) < 0)
        return error_tuple(env, strerror(errno));

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_binary(env, &ifr));
}


    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, char *err)
{
    return enif_make_tuple(env, 2,
            atom_error,
            enif_make_atom(env, err));
}


    static ERL_NIF_TERM
error_message(ErlNifEnv *env, char *err, char *msg)
{
    return enif_make_tuple(env, 2,
            atom_error,
            enif_make_tuple(env, 2,
            enif_make_atom(env, err),
            enif_make_string(env, msg, ERL_NIF_LATIN1)));
}


static ErlNifFunc nif_funcs[] = {
    {"open", 1, nif_open},
    {"poll", 1, nif_poll},
    {"close", 2, nif_close},
    {"bind", 2, nif_bind},
    {"recvfrom", 2, nif_recvfrom},
    {"ioctl", 3, nif_ioctl},
    {"sendto", 4, nif_sendto}
};

ERL_NIF_INIT(procket, nif_funcs, load, NULL, NULL, NULL)


