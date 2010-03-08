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

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *atom, char *err);
static ERL_NIF_TERM error_message(ErlNifEnv *env, char *atom, char *err, char *msg);


    static ERL_NIF_TERM
sock_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sock_fd = -1;
    struct sockaddr_un sa = { 0 };
    int flags = 0;


    if (enif_get_string(env, argv[0], sa.sun_path, sizeof(sa.sun_path), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    sa.sun_family = PF_LOCAL;

    errno = 0;
    sock_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (sock_fd < 0)
        return error_message(env, "error", "socket", strerror(errno));

    flags = fcntl(sock_fd, F_GETFL, 0);
    flags |= O_NONBLOCK;
    (void)fcntl(sock_fd, F_SETFL, flags);

    errno = 0;
    if (bind(sock_fd, (struct sockaddr *)&sa, sizeof(sa)) < 0)
        return error_message(env, "error", "bind", strerror(errno));

    errno = 0;
    if (listen(sock_fd, BACKLOG) < 0)
        return error_message(env, "error", "listen", strerror(errno));

    return enif_make_tuple(env, 2,
           enif_make_atom(env, "ok"),
           enif_make_int(env, sock_fd));
}


    static ERL_NIF_TERM
poll(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sock_fd = -1;        /* listening socket */
    int fd = -1;             /* connected socket */
    int s = -1;              /* socket received from pipe */
    struct sockaddr_un sa = { 0 };
    socklen_t socklen = 0;


    if (!enif_get_int(env, argv[0], &sock_fd))
        return enif_make_badarg(env);

    errno = 0;
    fd = accept(sock_fd, (struct sockaddr *)&sa, &socklen);
    if (fd < 0)
        return error_message(env, "error", "accept", strerror(errno));

    errno = 0;
    if (ancil_recv_fd(fd, &s) < 0) {
        (void)close (fd);
        return error_message(env, "error", "recvmsg", strerror(errno));
    }

    (void)close (fd);

    return enif_make_tuple(env, 2,
            enif_make_atom(env, "ok"),
            enif_make_int(env, s));
}


/* 0: path to socket
 * 1: file descriptor
 */
    static ERL_NIF_TERM
sock_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct sockaddr_un sa = { 0 };
    int sockfd = -1;


    if (enif_get_string(env, argv[0], sa.sun_path, sizeof(sa.sun_path), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &sockfd))
        return enif_make_badarg(env);

    errno = 0;
    if (unlink(sa.sun_path) < 0)
        return error_message(env, "error", "unlink", strerror(errno));

    (void)close(sockfd);

    return enif_make_atom(env, "ok");
}


    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, char *atom, char *err)
{
    return enif_make_tuple(env, 2,
            enif_make_atom(env, atom),
            enif_make_atom(env, err));
}


    static ERL_NIF_TERM
error_message(ErlNifEnv *env, char *atom, char *err, char *msg)
{
    return enif_make_tuple(env, 2,
            enif_make_atom(env, atom),
            enif_make_tuple(env, 2,
            enif_make_atom(env, err),
            enif_make_string(env, msg, ERL_NIF_LATIN1)));
}


static ErlNifFunc nif_funcs[] = {
    {"open", 1, sock_open},
    {"poll", 1, poll},
    {"close", 2, sock_close}
};

ERL_NIF_INIT(procket, nif_funcs, NULL, NULL, NULL, NULL)


