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
#include "erl_nif.h"
#include "erl_driver.h"
#include "ancillary.h"
#include "procket.h"
#ifdef HAVE_EV4
#include "ev.h"
#endif

#define BACKLOG     5

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum);
void alloc_free(ErlNifEnv *env, void *obj);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;

static ErlNifResourceType *PROCKET_ALLOC_RESOURCE;

#ifdef HAVE_EV4
static ErlNifResourceType *PROCKET_WATCHER_RESOURCE;

static struct event_loop {
    ErlNifTid       tid;
    ErlNifMutex    *mutex;
    struct ev_loop *env;
    ev_async        kick;
    ev_async        die;
} PROCKET_EVENT_LOOP;
#endif

typedef struct _alloc_state {
    size_t size;
    void *buf;
} ALLOC_STATE;

#ifdef HAVE_EV4
typedef struct _watcher_state {
    ev_io         io;
    ErlNifEnv    *env;
    ErlNifPid     pid;
    ERL_NIF_TERM  term;
} WATCHER_STATE;
#endif

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
#define PROCKET_REALLOC(bin, nsize) do { \
    size_t osize = bin.size; \
    if (nsize != bin.size) { \
        if (!enif_realloc_binary(&bin, nsize)) \
            return error_tuple(env, ENOMEM); \
        if (nsize > osize) \
            (void)memset(bin.data+osize, 0, bin.size-osize); \
    } \
} while (0);

#ifdef HAVE_EV4
/* libev processing
 * 
 * We set up a separate thread to listen for libev registered events
 *
 */
    static void
kick_cb(struct ev_loop *loop, ev_async *w, int revents)
{
    // just used for the side effects..this callback forces the loop
    // to re-eval the current set of watchers
}

    static void
die_cb(struct ev_loop *loop, ev_async *w, int revents)
{
    ev_break(loop, EVBREAK_ALL);
}

    static void
l_release(struct ev_loop *loop)
{
    struct event_loop *el = &PROCKET_EVENT_LOOP;
    enif_mutex_unlock(el->mutex);
}

    static void
l_acquire(struct ev_loop *loop)
{
    struct event_loop *el = &PROCKET_EVENT_LOOP;
    enif_mutex_lock(el->mutex);
}

    void*
event_loop(void *arg)
{
    struct event_loop *el = &PROCKET_EVENT_LOOP; 

    l_acquire (el->env);
    ev_run (el->env, 0);
    l_release (el->env);

    return NULL;
}

    static void
watcher_cb(struct ev_loop *loop, ev_io *w_, int revents)
{
    WATCHER_STATE *w = (WATCHER_STATE *)w_;
    struct event_loop *el = &PROCKET_EVENT_LOOP;
    ERL_NIF_TERM msg;
    
    msg = enif_make_tuple3(w->env,
                           enif_make_atom(w->env, "procket_watcher"),
                           enif_make_int(w->env, revents),
                           w->term
        );

    enif_send(NULL, &w->pid, w->env, msg);
    
    ev_io_stop(el->env, &w->io);
}

    void
watcher_free(ErlNifEnv *env, void *obj)
{
    WATCHER_STATE *w = obj;
    struct event_loop *el = &PROCKET_EVENT_LOOP;
    
    l_acquire(el->env);
    ev_io_stop(el->env, &w->io);
    ev_async_send(el->env, &el->kick);
    l_release(el->env);

    enif_free_env(w->env);
}
#endif

    static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_eagain = enif_make_atom(env, "eagain");

    if ( (PROCKET_ALLOC_RESOURCE = enif_open_resource_type(env, NULL,
        "procket_alloc_resource", alloc_free,
        ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

#if HAVE_EV4
    if ( (PROCKET_WATCHER_RESOURCE = enif_open_resource_type(env, NULL,
        "procket_watcher_resource", watcher_free,
        ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    struct event_loop *el = &PROCKET_EVENT_LOOP;

    memset(el, 0, sizeof(*el));

    el->mutex = enif_mutex_create("libev");
    if (!el->mutex)
        return -1;

    el->env = ev_default_loop(0);

    ev_async_init(&el->kick, kick_cb);
    ev_async_start(el->env, &el->kick);

    ev_async_init(&el->die, die_cb);
    ev_async_start(el->env, &el->die);

    ev_set_loop_release_cb(el->env, l_release, l_acquire);

    if (enif_thread_create("libev", &el->tid, event_loop, NULL, NULL) < 0)
        return -1;
#endif

    return (0);
}

    static void
unload(ErlNifEnv* env, void *priv_data)
{
#if HAVE_EV4
    struct event_loop *el = &PROCKET_EVENT_LOOP;

    /* send an asynchronous ev_break */
    l_acquire(el->env);
    ev_async_send(el->env, &el->die);
    l_release(el->env);
    
    /* block here before cleaning up remaining resources */
    enif_thread_join(el->tid, NULL);

    enif_mutex_destroy(el->mutex);
#endif
}

/* Retrieve the file descriptor from the forked privileged process */
/* 0: connected Unix socket */
    static ERL_NIF_TERM
nif_fdrecv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int fd = -1;    /* connected socket */
    int s = -1;     /* socket received from pipe */


    if (!enif_get_int(env, argv[0], &fd))
        return enif_make_badarg(env);

    if (ancil_recv_fd(fd, &s) < 0) {
        int err = errno;
        (void)close(fd);
        return error_tuple(env, err);
    }

    if (close(fd) < 0)
        return error_tuple(env, errno);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, s));
}


/*  0: procotol, 1: type, 2: family */
    static ERL_NIF_TERM
nif_socket(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
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

    if (fcntl(s, F_SETFL, flags|O_NONBLOCK) < 0)
        return error_tuple(env, errno);

    return enif_make_tuple2(env,
           atom_ok,
           enif_make_int(env, s));
}


/* 0: file descriptor, 1: backlog */
    static ERL_NIF_TERM
nif_listen(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
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
    static ERL_NIF_TERM
nif_accept(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int l = -1;
    int s = -1;
    int salen = 0;
    ErlNifBinary sa = {0};
    int flags = 0;


    if (!enif_get_int(env, argv[0], &l))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &salen))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(salen, &sa))
        return error_tuple(env, ENOMEM);

    s = accept(l, (sa.size == 0 ? NULL : (struct sockaddr *)sa.data), (socklen_t *)&salen);
    if (s < 0)
        return error_tuple(env, errno);

    flags = fcntl(s, F_GETFL, 0);

    if (flags < 0)
        return error_tuple(env, errno);

    if (fcntl(s, F_SETFL, flags|O_NONBLOCK) < 0)
        return error_tuple(env, errno);

    PROCKET_REALLOC(sa, salen);

    return enif_make_tuple3(env,
            atom_ok,
            enif_make_int(env, s),
            enif_make_binary(env, &sa));
}


/* 0: file descriptor */
    static ERL_NIF_TERM
nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sockfd = -1;


    if (!enif_get_int(env, argv[0], &sockfd))
        return enif_make_badarg(env);

    if (close(sockfd) < 0)
        return error_tuple(env, errno);

    return atom_ok;
}


/* 0: socket, 1: length */
/* 0: socket, 1: length, 2: flags, 3: struct sockaddr length */
    static ERL_NIF_TERM
nif_recvfrom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sockfd = -1;
    unsigned long len = 0;
    int salen = 0;
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
    if (!enif_get_int(env, argv[3], &salen))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(len, &buf))
        return error_tuple(env, ENOMEM);

    if (!enif_alloc_binary(salen, &sa))
        return error_tuple(env, ENOMEM);

    if ( (bufsz = recvfrom(sockfd, buf.data, buf.size, flags,
        (sa.size == 0 ? NULL : (struct sockaddr *)sa.data),
        (socklen_t *)&salen)) == -1) {
        enif_release_binary(&buf);
        enif_release_binary(&sa);
        switch (errno) {
            case EAGAIN:
            case EINTR:
                return enif_make_tuple2(env, atom_error, atom_eagain);
            default:
                return error_tuple(env, errno);
        }
    }

    PROCKET_REALLOC(buf, bufsz);
    PROCKET_REALLOC(sa, salen);

    return enif_make_tuple3(env, atom_ok, enif_make_binary(env, &buf),
             enif_make_binary(env, &sa));
}


/* 0: socket, 1: buffer, 2: flags, 3: struct sockaddr */
    static ERL_NIF_TERM
nif_sendto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int sockfd = -1;
    int flags = 0;

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

    if (sendto(sockfd, buf.data, buf.size, flags,
        (sa.size == 0 ? NULL : (struct sockaddr *)sa.data),
        sa.size) == -1)
        return error_tuple(env, errno);

    return atom_ok;
}


/* 0: socket, 1: length */
    static ERL_NIF_TERM
nif_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
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

    if ( (bufsz = read(fd, buf.data, buf.size)) == -1) {
        enif_release_binary(&buf);
        switch (errno) {
            case EAGAIN:
            case EINTR:
                return enif_make_tuple2(env, atom_error, atom_eagain);
            default:
                return error_tuple(env, errno);
        }
    }

    PROCKET_REALLOC(buf, bufsz);

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}


/* 0: fd, 1: buffer */
    static ERL_NIF_TERM
nif_write(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int fd = -1;

    ErlNifBinary buf = {0};

    if (!enif_get_int(env, argv[0], &fd))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &buf))
        return enif_make_badarg(env);

    if (write(fd, buf.data, buf.size) == -1)
        return error_tuple(env, errno);

    return atom_ok;
}

#define IOVMAX 256

/* 0: fd, 1: list of buffers */
    static ERL_NIF_TERM
nif_writev(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM head = {0};
    ERL_NIF_TERM tail = {0};
    struct iovec iovs[IOVMAX];
    int fd = -1;
    unsigned iovcnt;
    
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

    if (writev(fd, iovs, iovcnt) == -1)
        return error_tuple(env, errno);

    return atom_ok;
}

/* 0: socket descriptor, 1: struct sockaddr */
    static ERL_NIF_TERM
nif_bind(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
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
    static ERL_NIF_TERM
nif_connect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int s = -1;
    ErlNifBinary sa = {0};


    if (!enif_get_int(env, argv[0], &s))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &sa))
        return enif_make_badarg(env);

    if (connect(s, (sa.size == 0 ? NULL : (struct sockaddr *)sa.data), sa.size) < 0)
        return error_tuple(env, errno);

    return atom_ok;
}


/* 0: (int)socket descriptor, 1: (int)device dependent request,
 * 2: (char *)argp, pointer to structure
 */
    static ERL_NIF_TERM
nif_ioctl(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int s = -1;
    unsigned long req = 0;
    ErlNifBinary arg = {0};


    if (!enif_get_int(env, argv[0], &s))
        return enif_make_badarg(env);

    if (!enif_get_ulong(env, argv[1], &req))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[2], &arg))
        return enif_make_badarg(env);

    if (!enif_realloc_binary(&arg, arg.size))
        return enif_make_badarg(env);

    if (ioctl(s, req, arg.data) < 0)
        return error_tuple(env, errno);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &arg));
}


/* 0: int socket descriptor, 1: int level,
 * 2: int optname, 3: void *optval
 */
    static ERL_NIF_TERM
nif_setsockopt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
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


/* Allocate structures for ioctl
 *
 * Some ioctl request structures have a field pointing
 * to a user allocated buffer.
 */

/* 0: list */
    static ERL_NIF_TERM
nif_alloc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM head = {0};
    ERL_NIF_TERM tail = {0};

    int arity = 0;
    char key[MAXATOMLEN+1];  /* Includes terminating NULL */
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
            enif_realloc_binary(&req, req.size+bin.size);
            (void)memcpy(req.data+index, bin.data, bin.size);
        }
        else if (enif_get_tuple(env, head, &arity, &array)) {
            ALLOC_STATE *p = NULL;
            ERL_NIF_TERM res = {0};
            size_t val = 0;
            ErlNifBinary initial = {0};

            if ( (arity != 2) ||
                !enif_get_atom(env, array[0], key, sizeof(key), ERL_NIF_LATIN1) ||
                (strcmp(key, "ptr") != 0))
                return enif_make_badarg(env);

            if ( !(enif_get_ulong(env, array[1], (unsigned long *)&val) && val > 0) &&
                !(enif_inspect_binary(env, array[1], &initial) && initial.size > 0))
                return enif_make_badarg(env);

            val = (initial.size > 0) ? initial.size : val;

            p = enif_alloc_resource(PROCKET_ALLOC_RESOURCE, sizeof(ALLOC_STATE));

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

            if (!enif_realloc_binary(&req, req.size+sizeof(void *)))
                return error_tuple(env, ENOMEM);

            (void)memcpy(req.data+index, &p->buf, sizeof(void *));

            res = enif_make_resource(env, p);
            enif_release_resource(p);

            resources = enif_make_list_cell(env, res, resources);
        }
        else
            return enif_make_badarg(env);
    }

    return enif_make_tuple3(env, atom_ok,
            enif_make_binary(env, &req),
            resources);
}

/* 0: resource */
    static ERL_NIF_TERM
nif_buf(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ALLOC_STATE *p = NULL;

    ErlNifBinary buf = {0};

    if (!enif_get_resource(env, argv[0], PROCKET_ALLOC_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(p->size, &buf))
        return error_tuple(env, ENOMEM);

    (void)memcpy(buf.data, p->buf, buf.size);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &buf));
}

/* 0: resource, 1: binary */
    static ERL_NIF_TERM
nif_memcpy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ALLOC_STATE *p = NULL;
    ErlNifBinary buf = {0};


    if (!enif_get_resource(env, argv[0], PROCKET_ALLOC_RESOURCE, (void **)&p))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &buf) || buf.size > p->size)
        return enif_make_badarg(env);

    (void)memcpy(p->buf, buf.data, buf.size);

    return atom_ok;
}


/* 0: errno */
    static ERL_NIF_TERM
nif_errno_id(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int err = -1;


    if (!enif_get_int(env, argv[0], &err))
        return enif_make_badarg(env);

    return enif_make_atom(env, erl_errno_id(err));
}

#ifdef HAVE_EV4
   static void
watcher_arm(WATCHER_STATE *w)
{
    struct event_loop *el = &PROCKET_EVENT_LOOP;

    l_acquire(el->env);
    ev_io_start(el->env, &w->io);
    ev_async_send(el->env, &el->kick);
    l_release(el->env);
} 

/* 0: fd, 1: flags, 2: term */
    static ERL_NIF_TERM
nif_watcher_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    WATCHER_STATE *w = NULL;
    ERL_NIF_TERM ret = enif_make_badarg(env);
    int fd;
    int flags;

    if (!enif_get_int(env, argv[0], &fd))
      goto out;

    if (!enif_get_int(env, argv[1], &flags))
      goto out;

    if (flags & ~(EV_READ | EV_WRITE))
      goto out;

    w = enif_alloc_resource(PROCKET_WATCHER_RESOURCE, sizeof(WATCHER_STATE));
    if (w == NULL)
      return error_tuple(env, ENOMEM);

    w->env = enif_alloc_env();
    if (!w->env) {
      ret = error_tuple(env, ENOMEM);
      goto out;
    }

    enif_self(env, &w->pid);
    w->term = enif_make_copy(w->env, argv[2]);

    ev_io_init(&w->io, watcher_cb, fd, flags);
    watcher_arm(w);

    ret = enif_make_tuple2(env, atom_ok, enif_make_resource(env, w));

 out:
    if (w)
      enif_release_resource(w);

    return ret;

}

/* 0: watcher */
    static ERL_NIF_TERM
nif_watcher_arm(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    WATCHER_STATE *w;

    if(!enif_get_resource(env, argv[0], PROCKET_WATCHER_RESOURCE, (void**)&w))
      return enif_make_badarg(env);
 
    watcher_arm(w);

    return atom_ok;
}

/* 0: watcher */
    static ERL_NIF_TERM
nif_watcher_disarm(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    struct event_loop *el = &PROCKET_EVENT_LOOP;
    WATCHER_STATE *w;

    if(!enif_get_resource(env, argv[0], PROCKET_WATCHER_RESOURCE, (void**)&w))
      return enif_make_badarg(env);

    l_acquire(el->env);
    ev_io_stop(el->env, &w->io);
    ev_async_send(el->env, &el->kick);
    l_release(el->env);

    return atom_ok;
}
#endif

    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, int errnum)
{
    return enif_make_tuple(env, 2,
            atom_error,
            enif_make_atom(env, erl_errno_id(errnum)));
}


    void
alloc_free(ErlNifEnv *env, void *obj)
{
    ALLOC_STATE *p = obj;

    if (p->buf == NULL)
        return;

    free(p->buf);
    p->buf = NULL;
    p->size = 0;
}


static ErlNifFunc nif_funcs[] = {
    {"fdrecv", 1, nif_fdrecv},

    {"close", 1, nif_close},
    {"accept", 2, nif_accept},
    {"bind", 2, nif_bind},
    {"connect", 2, nif_connect},
    {"listen", 2, nif_listen},
    {"ioctl", 3, nif_ioctl},
    {"socket_nif", 3, nif_socket},
    {"recvfrom", 4, nif_recvfrom},
    {"sendto", 4, nif_sendto},
    {"setsockopt", 4, nif_setsockopt},

    {"read", 2, nif_read},
    {"write", 2, nif_write},
    {"writev", 2, nif_writev},

    {"alloc", 1, nif_alloc},
    {"memcpy", 2, nif_memcpy},
    {"buf", 1, nif_buf},

#ifdef HAVE_EV4
    {"watcher_create", 3, nif_watcher_create},
    {"watcher_arm", 1, nif_watcher_arm},
    {"watcher_disarm", 1, nif_watcher_disarm},
#endif

    {"errno_id", 1, nif_errno_id}
};

ERL_NIF_INIT(procket, nif_funcs, load, NULL, NULL, unload)
