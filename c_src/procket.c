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

typedef struct {
    int fd;
    char *path;
} PRIVDATA;

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *atom, char *err);
static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char *buf, size_t buflen);
static ERL_NIF_TERM error_message(ErlNifEnv *env, char *atom, char *err, char *msg);
static ERL_NIF_TERM sock_close(ErlNifEnv *env);


    static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    PRIVDATA *data = NULL;


    data = (PRIVDATA *)enif_alloc(env, sizeof(PRIVDATA));
    if (data == NULL)
        return (-1);

    (void)memset(data, '\0', sizeof(PRIVDATA));
    *priv = data;

    return (0);
}   


    static int
reload(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    (void)sock_close(env);
    enif_free(env, ((PRIVDATA *)*priv)->path);
    enif_free(env, *priv);

    return load(env, priv, load_info);
}


    static ERL_NIF_TERM
sock_open(ErlNifEnv *env, ERL_NIF_TERM path, ERL_NIF_TERM protocol)
{
    PRIVDATA *data = NULL;
    int proto = 0;
    struct sockaddr_un sa = { 0 };
    int flags = 0;


    data = (PRIVDATA *)enif_get_data(env);

    if (data->path != NULL)
        return error_tuple(env, "error", "socket_already_open");

    data->path = (char *)enif_alloc(env, UNIX_PATH_MAX);
    if (data->path == NULL)
        return error_tuple(env, "error", "memory_allocation_failure");

    if (!my_enif_get_string(env, path, data->path, UNIX_PATH_MAX))
        return enif_make_badarg(env);

    if (strlen(data->path) == 0)
        return enif_make_badarg(env);

    if (!enif_get_int(env, protocol, &proto))
        return enif_make_badarg(env);

    sa.sun_family = PF_LOCAL;
    (void)memcpy(sa.sun_path, data->path, sizeof(sa.sun_path)-1);

    errno = 0;
    data->fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (data->fd < 0)
        return error_message(env, "error", "socket", strerror(errno));

    flags = fcntl(data->fd, F_GETFL, 0);
    flags |= O_NONBLOCK;
    (void)fcntl(data->fd, F_SETFL, flags);

    errno = 0;
    if (bind(data->fd, (struct sockaddr *)&sa, sizeof(sa)) < 0)
        return error_message(env, "error", "bind", strerror(errno));

    errno = 0;
    if (listen(data->fd, 5) < 0)
        return error_message(env, "error", "listen", strerror(errno));

    return enif_make_atom(env, "ok");
}


    static ERL_NIF_TERM
poll(ErlNifEnv *env)
{
    PRIVDATA *data = NULL;
    int s= 0;
    int pipefd = 0;
    struct sockaddr_un sa = { 0 };
    socklen_t socklen = 0;


    data = (PRIVDATA *)enif_get_data(env);

    if (data->path == NULL)
        return error_tuple(env, "error", "no_socket");

    errno = 0;
    pipefd = accept(data->fd, (struct sockaddr *)&sa, &socklen);
    if (pipefd < 0)
        return error_message(env, "error", "accept", strerror(errno));

    errno = 0;
    if (ancil_recv_fd(pipefd, &s) < 0) {
        (void)close (pipefd);
        return error_message(env, "error", "recvmsg", strerror(errno));
    }

    (void)close (pipefd);
    (void)sock_close(env);

    return enif_make_tuple(env, 2,
            enif_make_atom(env, "ok"),
            enif_make_int(env, s));
}


    static ERL_NIF_TERM
sock_close(ErlNifEnv *env)
{
    PRIVDATA *data = NULL;


    data = (PRIVDATA *)enif_get_data(env);

    if (data->path == NULL)
        return error_tuple(env, "error", "no_socket");

    (void)close(data->fd);

    errno = 0;
    if (unlink(data->path) < 0)
        return error_message(env, "error", "unlink", strerror(errno));

    if (data->path) {
        enif_free(env, data->path);
        data->path = NULL;
    }

    data->fd = -1;

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
            enif_make_string(env, msg)));
}


/* from:
 * http://d.hatena.ne.jp/vostok92/20091201/1259680319
 */
    static int
my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char *buf, size_t buflen)
{
    ERL_NIF_TERM head, tail;
    int val;
    int n = 1;


    while ((n++ < buflen) && (enif_get_list_cell(env, list, &head, &tail))) {
        if (!enif_get_int(env, head, &val))
            return 0;

        *buf = (char)val;
        buf++;
        list = tail; 
    }
    *buf = '\0';

    return 1;
}

static ErlNifFunc nif_funcs[] = {
    {"open", 2, sock_open},
    {"poll", 0, poll},
    {"close", 0, sock_close}
};

ERL_NIF_INIT(procket, nif_funcs, load, reload, NULL, NULL)


