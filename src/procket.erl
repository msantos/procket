%%% @copyright 2010-2023 Michael Santos <michael.santos@gmail.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(procket).
-include("procket.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    open/1, open/2,
    dev/1, dev/2,
    socket/3,
    listen/1, listen/2,
    connect/2,
    accept/1, accept/2,
    close/1,
    recv/2,
    recvfrom/2, recvfrom/4,
    sendto/2, sendto/3, sendto/4,
    read/2,
    write/2,
    writev/2,
    bind/2,
    ioctl/3,
    setsockopt/4,
    getsockopt/4,
    getsockname/2,

    recvmsg/4, recvmsg/5,
    sendmsg/4, sendmsg/5,

    setns/1, setns/2,

    family/1,

    alloc/1,
    buf/1,
    memcpy/2,
    wordalign/1, wordalign/2,

    socket_level/0, socket_level/1,
    socket_optname/0, socket_optname/1,
    socket_protocol/0, socket_protocol/1,

    errno_id/1,

    set_sock_nonblock/1
]).
-export([
    unix_path_max/0,
    sockaddr_common/2,
    ntohl/1,
    ntohs/1
]).
% for debugging
-export([
    getopts/1,
    progname/0
]).

-on_load(on_load/0).

on_load() ->
    erlang:load_nif(progname(), []).

%%--------------------------------------------------------------------
%%% NIF Stubs
%%--------------------------------------------------------------------
close(_) ->
    erlang:nif_error(not_implemented).

fdrecv(_) ->
    erlang:nif_error(not_implemented).

accept(Socket) ->
    case accept(Socket, 0) of
        {ok, FD, <<>>} -> {ok, FD};
        Error -> Error
    end.
accept(_, _) ->
    erlang:nif_error(not_implemented).

bind(_, _) ->
    erlang:nif_error(not_implemented).

connect(_, _) ->
    erlang:nif_error(not_implemented).

listen(Socket) when is_integer(Socket) ->
    listen(Socket, ?BACKLOG).
listen(_, _) ->
    erlang:nif_error(not_implemented).

recv(Socket, Size) ->
    recvfrom(Socket, Size).
recvfrom(Socket, Size) ->
    case recvfrom(Socket, Size, 0, 0) of
        {ok, Buf, <<>>} -> {ok, Buf};
        Error -> Error
    end.
recvfrom(_, _, _, _) ->
    erlang:nif_error(not_implemented).

read(_, _) ->
    erlang:nif_error(not_implemented).

socket(Family, Type, Protocol) ->
    socket_nif(
        maybe_atom(family, Family),
        maybe_atom(type, Type),
        maybe_atom(protocol, Protocol)
    ).
socket_nif(_, _, _) ->
    erlang:nif_error(not_implemented).

setns(NS) ->
    setns(NS, 0).
setns(_, _) ->
    erlang:nif_error(not_implemented).

ioctl(_, _, _) ->
    erlang:nif_error(not_implemented).
buf(_) ->
    erlang:nif_error(not_implemented).
memcpy(_, _) ->
    erlang:nif_error(not_implemented).

alloc(Struct) ->
    case alloc_nif(Struct) of
        {ok, Bin, Res} ->
            {ok, Bin, lists:reverse(Res)};
        N ->
            N
    end.
alloc_nif(_) ->
    erlang:nif_error(not_implemented).

sendto(Socket, Buf) ->
    sendto(Socket, Buf, 0, <<>>).
sendto(Socket, Buf, Flags) ->
    sendto(Socket, Buf, Flags, <<>>).
sendto(Socket, Buf, Flags, Sockaddr) ->
    Size = byte_size(Buf),
    case sendto_nif(Socket, Buf, Flags, Sockaddr) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

sendto_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).

write(FD, Buf) when is_binary(Buf) ->
    Size = byte_size(Buf),
    case write_nif(FD, Buf) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end;
write(FD, Buf) when is_list(Buf) ->
    writev(FD, Buf).

write_nif(_, _) ->
    erlang:nif_error(not_implemented).

writev(FD, Buf) ->
    Size = iolist_size(Buf),
    case writev_nif(FD, Buf) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

writev_nif(_, _) ->
    erlang:nif_error(not_implemented).

recvmsg(Socket, Size, Flags, CtrlDataSize) ->
    case recvmsg(Socket, Size, Flags, CtrlDataSize, 0) of
        {ok, Buf, Flags, CtrlData, <<>>} ->
            {ok, Buf, Flags, CtrlData};
        {error, _} = Error ->
            Error
    end.
recvmsg(Socket, Size, Flags, CtrlDataSize, SockaddrSize) ->
    case recvmsg_nif(Socket, Size, Flags, CtrlDataSize, SockaddrSize) of
        {ok, Buf, Flags, CtrlData, Sockaddr} ->
            {ok, Buf, Flags, lists:reverse(CtrlData), Sockaddr};
        N ->
            N
    end.
recvmsg_nif(_, _, _, _, _) ->
    erlang:nif_error(not_implemented).

sendmsg(Socket, Buf, Flags, CtrlData) ->
    sendmsg(Socket, Buf, Flags, CtrlData, <<>>).
sendmsg(Socket, Buf, Flags, CtrlData, Sockaddr) ->
    Size = byte_size(Buf),
    case sendmsg_nif(Socket, Buf, Flags, CtrlData, Sockaddr) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

sendmsg_nif(_, _, _, _, _) ->
    erlang:nif_error(not_implemented).

setsockopt(Socket, Level, Optname, Optval) when is_atom(Level) ->
    case
        socket_constant_foreach(
            Level,
            [fun socket_level/1, fun socket_protocol/1]
        )
    of
        undefined ->
            {error, unsupported};
        N ->
            setsockopt(Socket, N, Optname, Optval)
    end;
setsockopt(Socket, Level, Optname, Optval) when is_atom(Optname) ->
    case socket_optname(Optname) of
        undefined ->
            {error, unsupported};
        N ->
            setsockopt(Socket, Level, N, Optval)
    end;
setsockopt(Socket, Level, Optname, Optval) ->
    setsockopt_nif(Socket, Level, Optname, Optval).

getsockopt(Socket, Level, Optname, Optval) when is_atom(Level) ->
    case
        socket_constant_foreach(
            Level,
            [fun socket_level/1, fun socket_protocol/1]
        )
    of
        undefined ->
            {error, unsupported};
        N ->
            getsockopt(Socket, N, Optname, Optval)
    end;
getsockopt(Socket, Level, Optname, Optval) when is_atom(Optname) ->
    case socket_optname(Optname) of
        undefined ->
            {error, unsupported};
        N ->
            getsockopt(Socket, Level, N, Optval)
    end;
getsockopt(Socket, Level, Optname, Optval) ->
    getsockopt_nif(Socket, Level, Optname, Optval).

setsockopt_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).
getsockopt_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).

getsockname(_, _) ->
    erlang:nif_error(not_implemented).

socket_level() ->
    erlang:nif_error(not_implemented).
socket_level(_) ->
    erlang:nif_error(not_implemented).

socket_optname() ->
    erlang:nif_error(not_implemented).
socket_optname(_) ->
    erlang:nif_error(not_implemented).

socket_protocol() ->
    erlang:nif_error(not_implemented).
socket_protocol(_) ->
    erlang:nif_error(not_implemented).

errno_id(_) ->
    erlang:nif_error(not_implemented).

set_sock_nonblock(_) ->
    erlang:nif_error(not_implemented).

socket_constant_foreach(_Constant, []) ->
    undefined;
socket_constant_foreach(Constant, [Fun | Funs]) ->
    case Fun(Constant) of
        undefined ->
            socket_constant_foreach(Constant, Funs);
        N when is_integer(N) ->
            N
    end.

%%--------------------------------------------------------------------
%%% Setuid helper
%%--------------------------------------------------------------------
dev(Dev) when is_list(Dev) ->
    open(0, [{dev, Dev}]).

dev(Dev, Opts) when is_list(Dev), is_list(Opts) ->
    open(0, [{dev, Dev} | Opts]).

open(Port) ->
    open(Port, []).
open(Port, Options) when is_integer(Port), is_list(Options) ->
    {Tmpdir, Pipe} = make_unix_socket_path(Options),
    {ok, FD} = fdopen(Pipe),

    Cmd = getopts(
        [
            {port, Port},
            {pipe, Pipe}
        ] ++ Options
    ),

    Socket = exec(FD, Cmd),
    close(FD),

    cleanup_unix_socket(Tmpdir, Pipe),
    Socket.

% Run the setuid helper
exec(FD, Cmd) ->
    exec(FD, Cmd, {error, enoent}).

exec(_FD, [], Errno) ->
    Errno;
exec(FD, [Cmd | Rest], _LastErrno) ->
    Proc = open_port({spawn, Cmd}, [exit_status]),
    ExitValue =
        receive
            {Proc, {exit_status, 0}} ->
                ok;
            {Proc, {exit_status, 127}} ->
                {error, enoent};
            {Proc, {exit_status, Status}} ->
                {error, errno_id(Status)}
        end,

    case ExitValue of
        ok ->
            fdget(FD);
        {error, N} = Errno when N =:= eacces; N =:= enoent; N =:= eperm ->
            exec(FD, Rest, Errno);
        Errno ->
            Errno
    end.

% Unix socket handling: retrieves the fd from the setuid helper
make_unix_socket_path(Options) ->
    {Tmpdir, Socket} =
        case proplists:get_value(pipe, Options) of
            undefined ->
                Tmp = procket_mktmp:dirname(),
                ok = procket_mktmp:make_dir(Tmp),
                Path = Tmp ++ "/sock",
                {Tmp, Path};
            Path ->
                {false, Path}
        end,
    {Tmpdir, Socket}.

cleanup_unix_socket(false, Pipe) ->
    prim_file:delete(Pipe);
cleanup_unix_socket(Tmpdir, Pipe) ->
    prim_file:delete(Pipe),
    procket_mktmp:close(Tmpdir).

fdopen(Path) when is_list(Path) ->
    fdopen(list_to_binary(Path));
fdopen(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = socket(family(local), type(stream), 0),
    Len = byte_size(Path),
    Sun = <<
        (sockaddr_common(family(local), Len))/binary, Path/binary, 0:((unix_path_max() - Len) * 8)
    >>,
    ok = bind(Socket, Sun),
    ok = listen(Socket, 0),
    {ok, Socket}.

fdget(Socket) ->
    {ok, S} = accept(Socket),
    fdrecv(S).

% Construct the cli arguments for the helper
getopts(Options) ->
    Exec = proplists:get_value(exec, Options, ["", "sudo"]),
    Progname = proplists:get_value(progname, Options, progname()),

    Args = join([optarg(Arg) || Arg <- Options]),
    Redirect = "> /dev/null 2>&1",

    [join([E, Progname, Args, Redirect]) || E <- Exec].

join(StringList) ->
    string:join([N || N <- StringList, N =/= ""], " ").

optarg({backlog, Arg}) ->
    switch("b", Arg);
optarg({pipe, Arg}) ->
    switch("u", Arg);
optarg({protocol, Proto}) when is_atom(Proto) ->
    optarg({protocol, protocol(Proto)});
optarg({protocol, Proto}) when is_integer(Proto) ->
    switch("P", Proto);
optarg({type, Type}) when is_atom(Type) ->
    optarg({type, type(Type)});
optarg({type, Type}) when is_integer(Type) ->
    switch("T", Type);
optarg({family, Family}) when is_atom(Family) ->
    optarg({family, family(Family)});
optarg({family, Family}) when is_integer(Family) ->
    switch("F", Family);
optarg({ip, Arg}) when is_tuple(Arg) -> inet_parse:ntoa(Arg);
optarg({ip, Arg}) when is_list(Arg) -> Arg;
optarg({port, Port}) when is_integer(Port) ->
    switch("p", Port);
optarg({interface, Name}) when is_list(Name) ->
    case is_interface(Name) of
        true ->
            switch("I", Name);
        false ->
            erlang:error(badarg, [{interface, Name}])
    end;
optarg({dev, Dev}) when is_list(Dev) ->
    case is_device(Dev) of
        true ->
            switch("d", Dev);
        false ->
            erlang:error(badarg, [{dev, Dev}])
    end;
optarg({namespace, NS}) when is_list(NS) ->
    switch("N", NS);
% Ignore any other arguments
optarg(_Arg) ->
    "".

switch(Switch, Arg) ->
    lists:concat(["-", Switch, " ", Arg]).

is_interface(Name) when is_list(Name) ->
    % An interface name is expected to consist of a reasonable
    % subset of all characters, use a whitelist and extend it if needed
    Name ==
        [
            C
         || C <- Name,
            (((C bor 32) >= $a) and ((C bor 32) =< $z)) or
                ((C >= $0) and (C =< $9)) or (C == $.)
        ].

is_device(Name) when is_list(Name) ->
    Name ==
        [
            C
         || C <- Name,
            ((C >= $a) and (C =< $z)) or
                ((C >= $0) and (C =< $9) or (C == $/))
        ].

progname_ebin() ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        ?MODULE
    ]).

progname_priv() ->
    case application:get_env(?MODULE, port_executable) of
        {ok, Executable} ->
            Executable;
        undefined ->
            filename:join([
                code:priv_dir(?MODULE),
                ?MODULE
            ])
    end.

progname() ->
    % Is there a proper way of getting App-Name in this context?
    case code:priv_dir(?MODULE) of
        {error, bad_name} -> progname_ebin();
        _ -> progname_priv()
    end.

%% Protocol family (aka domain)
family(unspec) ->
    0;
family(inet) ->
    2;
family(inet6) ->
    case os:type() of
        {unix, linux} -> 10;
        {unix, darwin} -> 30;
        {unix, freebsd} -> 28;
        {unix, netbsd} -> 24;
        {unix, openbsd} -> 24;
        {unix, sunos} -> 26
    end;
family(netlink) ->
    16;
family(packet) ->
    17;
family(Proto) when Proto == local; Proto == unix; Proto == file -> 1.

%% Socket type
type(stream) ->
    case os:type() of
        {unix, sunos} -> 2;
        {unix, _} -> 1
    end;
type(dgram) ->
    case os:type() of
        {unix, sunos} -> 1;
        {unix, _} -> 2
    end;
type(raw) ->
    case os:type() of
        {unix, sunos} -> 4;
        {unix, _} -> 3
    end;
type(seqpacket) ->
    5.

% Select a protocol within the family (0 means use the default
% protocol in the family)
protocol(ip) -> 0;
protocol(icmp) -> 1;
protocol(tcp) -> 6;
protocol(udp) -> 17;
protocol(ipv6) -> 41;
protocol(icmp6) -> 58;
protocol('ipv6-icmp') -> 58;
protocol(raw) -> 255.

maybe_atom(_Type, Value) when is_integer(Value) -> Value;
maybe_atom(family, Value) -> family(Value);
maybe_atom(type, Value) -> type(Value);
maybe_atom(protocol, Value) -> protocol(Value).

%%
%% Portability
%%

% struct sockaddr
sockaddr_common(Family0, Length) ->
    Family = maybe_atom(family, Family0),
    case erlang:system_info(os_type) of
        {unix, BSD} when
            BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd
        ->
            <<Length:8, Family:8>>;
        {unix, _} ->
            <<Family:16/native>>
    end.

% UNIX_PATH_MAX
unix_path_max() ->
    case erlang:system_info(os_type) of
        {unix, BSD} when
            BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd
        ->
            104;
        {unix, _} ->
            108
    end.

ntohl(<<I:32>>) ->
    ntohl(I);
ntohl(I) when is_integer(I) ->
    <<N:32>> = <<I:32/native>>,
    N.
ntohs(<<I:32>>) ->
    ntohs(I);
ntohs(I) when is_integer(I) ->
    <<N:16>> = <<I:16/native>>,
    N.

wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
wordalign(Offset, Align) ->
    (Align - (Offset rem Align)) rem Align.
