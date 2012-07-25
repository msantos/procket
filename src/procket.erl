%% Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%% 
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%% 
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(procket).
-include("procket.hrl").

-export([
        init/0,
        open/1,open/2,
        dev/1,
        socket/3,
        listen/1,listen/2,
        connect/2,
        accept/1,accept/2,
        close/1,
        recv/2,recvfrom/2,recvfrom/4,
        sendto/2, sendto/3,sendto/4,
        read/2, write/2, writev/2,
        bind/2,
        ioctl/3,
        setsockopt/4,
        getsockname/2,

        alloc/1,
        buf/1,
        memcpy/2,
        wordalign/1, wordalign/2,

        errno_id/1
    ]).
-export([
        unix_path_max/0,
        sockaddr_common/2,
        ntohl/1,
        ntohs/1
    ]).

-on_load(on_load/0).


init() ->
    on_load().

on_load() ->
    erlang:load_nif(progname(), []).



close(_) ->
    erlang:error(not_implemented).

fdrecv(_) ->
    erlang:error(not_implemented).

accept(Socket) ->
    case accept(Socket, 0) of
        {ok, FD, <<>>} -> {ok, FD};
        Error -> Error
    end.
accept(_,_) ->
    erlang:error(not_implemented).

bind(_,_) ->
    erlang:error(not_implemented).

connect(_,_) ->
    erlang:error(not_implemented).

listen(Socket) when is_integer(Socket) ->
    listen(Socket, ?BACKLOG).
listen(_,_) ->
    erlang:error(not_implemented).

recv(Socket,Size) ->
    recvfrom(Socket,Size).
recvfrom(Socket,Size) ->
    case recvfrom(Socket, Size, 0, 0) of
        {ok, Buf, <<>>} -> {ok, Buf}; 
        Error -> Error
    end.
recvfrom(_,_,_,_) ->
    erlang:error(not_implemented).

read(_,_) ->
    erlang:error(not_implemented).

socket(Family, Type, Protocol) ->
    socket_nif(maybe_atom(family, Family),
        maybe_atom(type, Type),
        maybe_atom(protocol, Protocol)).
socket_nif(_,_,_) ->
    erlang:error(not_implemented).

ioctl(_,_,_) ->
    erlang:error(not_implemented).
alloc(_) ->
    erlang:error(not_implemented).
buf(_) ->
    erlang:error(not_implemented).
memcpy(_,_) ->
    erlang:error(not_implemented).

sendto(Socket, Buf) ->
    sendto(Socket, Buf, 0, <<>>).
sendto(Socket, Buf, Flags) ->
    sendto(Socket, Buf, Flags, <<>>).
sendto(_,_,_,_) ->
    erlang:error(not_implemented).

write(_,_) ->
    erlang:error(not_implemented).
writev(_,_) ->
    erlang:error(not_implemented).

setsockopt(_,_,_,_) ->
    erlang:error(not_implemented).

getsockname(_,_) ->
    erlang:error(not_implemented).

errno_id(_) ->
    erlang:error(not_implemented).


dev(Dev) when is_list(Dev) ->
    open(0, [{dev, Dev}]).

open(Port) ->
    open(Port, []).
open(Port, Options) when is_integer(Port), is_list(Options) ->
    Opt = case proplists:get_value(pipe, Options) of
        undefined ->
            Tmp = procket_mktmp:dirname(),
            ok = procket_mktmp:make_dir(Tmp),
            Path = Tmp ++ "/sock",
            [{pipe, Path}, {tmpdir, Tmp}] ++ Options;
        _ ->
            [{tmpdir, false}] ++ Options
    end,
    Result = open_1(Port, Opt),
    case proplists:get_value(tmpdir, Options) of
        false ->
            ok;
        Tmp2 ->
            procket_mktmp:close(Tmp2)
    end,
    Result.

open_1(Port, Options) ->
    case open_1(Port, Options, false) of
        {error, eacces} ->
            open_1(Port, Options, true);
        Result ->
            Result
    end.

open_1(Port, Options, UseSudo) ->
    Pipe = proplists:get_value(pipe, Options),
    {ok, Sockfd} = fdopen(Pipe),
    Cmd = make_args(Port, Options, UseSudo),
    case os:cmd(Cmd) of
        "0" ->
            FD = fdget(Sockfd),
            cleanup(Sockfd, Pipe, Options),
            FD;
        Error ->
            cleanup(Sockfd, Pipe, Options),
            {error, errno_id(list_to_integer(Error))}
    end.

cleanup(Sockfd, Pipe, Options) ->
    close(Sockfd),
    ok = file:delete(Pipe).

fdopen(Path) when is_list(Path) ->
    fdopen(list_to_binary(Path));
fdopen(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = socket(?PF_LOCAL, ?SOCK_STREAM, 0),
    Len = byte_size(Path),
    Sun = <<(sockaddr_common(?PF_LOCAL, Len))/binary,
        Path/binary,
        0:((unix_path_max()-Len)*8)
        >>,
    ok = bind(Socket, Sun),
    ok = listen(Socket, ?BACKLOG),
    {ok, Socket}.

fdget(Socket) ->
    {ok, S} = accept(Socket),
    fdrecv(S).

make_args(Port, Options, UseSudo) ->
    Args = reorder_args(Port, Options),
    Prefix = case UseSudo of
        true ->
            "sudo ";
        false ->
            ""
    end,
    proplists:get_value(progname, Options, Prefix ++ progname()) ++ " " ++
    string:join([ get_switch(Arg) || Arg <- Args ], " ") ++
    " > /dev/null 2>&1; printf $?".

reorder_args(Port, Options) ->
    NewOpts = case proplists:lookup(ip, Options) of
        none ->
            Options;
        IP ->
            proplists:delete(ip, Options) ++ [IP]
    end,
    [{port, Port}] ++ NewOpts.

get_switch({pipe, Arg}) ->
    "-u " ++ Arg;

get_switch({protocol, Proto}) when is_atom(Proto) ->
    get_switch({protocol, protocol(Proto)});
get_switch({protocol, Proto}) when is_integer(Proto) ->
    "-P " ++ integer_to_list(Proto);

get_switch({type, Type}) when is_atom(Type) ->
    get_switch({type, type(Type)});
get_switch({type, Type}) when is_integer(Type) ->
    "-T " ++ integer_to_list(Type);

get_switch({family, Family}) when is_atom(Family) ->
    get_switch({family, family(Family)});
get_switch({family, Family}) when is_integer(Family) ->
    "-F " ++ integer_to_list(Family);

get_switch({ip, Arg}) when is_tuple(Arg) -> inet_parse:ntoa(Arg);
get_switch({ip, Arg}) when is_list(Arg) -> Arg;

get_switch({port, Port}) when is_integer(Port) ->
    "-p " ++ integer_to_list(Port);

get_switch({interface, Name}) when is_list(Name) ->
    case is_interface(Name) of
        true ->
            "-I " ++ Name;
        false ->
            throw({bad_interface, Name})
    end;

get_switch({dev, Dev}) when is_list(Dev) ->
    case is_device(Dev) of
        true ->
            "-d " ++ Dev;
        false ->
            throw({bad_device, Dev})
    end;

% Ignore any other arguments
get_switch(_Arg) ->
    "".

is_interface(Name) when is_list(Name) ->
    % An interface name is expected to consist of a reasonable
    % subset of all characters, use a whitelist and extend it if needed
    Name == [C || C <- Name, (((C bor 32) >= $a) and ((C bor 32) =< $z))
        or ((C >= $0) and (C =< $9)) or (C == $.)].

is_device(Name) when is_list(Name) ->
    Name == [C || C <- Name, ((C >= $a) and (C =< $z))
        or ((C >= $0) and (C =< $9) or (C == $/))].

progname_ebin() ->
    filename:join([
        filename:dirname( code:which( ?MODULE ) ),
        "..", "priv", ?MODULE
    ]).

progname_priv() ->
    case application:get_env( ?MODULE, port_executable ) of
        {ok, Executable} -> Executable;
        undefined -> filename:join([
                            code:priv_dir( ?MODULE ), 
                            ?MODULE
                        ])
    end.

progname() ->
    % Is there a proper way of getting App-Name in this context?
    case code:priv_dir( ?MODULE ) of
        {error,bad_name} -> progname_ebin();
        ________________ -> progname_priv()
    end.

%% Protocol family (aka domain)
family(unspec) -> 0;
family(inet) -> 2;
family(inet6) ->
    case os:type() of
        {unix, linux} -> 10;
        {unix, darwin} -> 30;
        {unix, freebsd} -> 28
    end;
family(netlink) -> 16;
family(packet) -> 17;
family(Proto) when Proto == local; Proto == unix; Proto == file -> 1;

family(0) -> unspec;
family(1) -> unix;
family(2) -> inet;
family(10) ->
    case os:type() of
        {unix, linux} -> inet6;
        {unix, _} -> ccitt
    end;
family(16) -> netlink;
family(17) ->
    case os:type() of
        {unix, linux} -> packet;
        {unix, _} -> route
    end;
family(28) ->
    case os:type() of
        % linux: not defined
        {unix, freebsd} -> inet6;
        {unix, darwin} -> isdn
    end;
family(30) ->
    case os:type() of
        {unix, linux} -> tipc;
        {unix, freebsd} -> atm;
        {unix, darwin} -> inet6
    end.


%% Socket type
type(stream) -> 1;
type(dgram) -> 2;
type(raw) -> 3;

type(1) -> stream;
type(2) -> dgram;
type(3) -> raw.


% Select a protocol within the family (0 means use the default
% protocol in the family)
protocol(ip) -> 0;
protocol(icmp) -> 1;
protocol(tcp) -> 6;
protocol(udp) -> 17;
protocol(icmp6) -> 58;
protocol('ipv6-icmp') -> 58;
protocol(raw) -> 255;

protocol(0) -> ip;
protocol(1) -> icmp;
protocol(6) -> tcp;
protocol(17) -> udp;
protocol(58) -> 'ipv6-icmp';
protocol(255) -> raw.

maybe_atom(_Type, Value) when is_integer(Value) -> Value;
maybe_atom(family, Value) -> family(Value);
maybe_atom(type, Value) -> type(Value);
maybe_atom(protocol, Value) -> protocol(Value).


%%
%% Portability
%%

% struct sockaddr
sockaddr_common(Family, Length) ->
    case erlang:system_info(os_type) of
        {unix,BSD} when BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd ->
            <<Length:8, Family:8>>;
        {unix,_} ->
            <<Family:16/native>>
    end.

% UNIX_PATH_MAX
unix_path_max() ->
    case erlang:system_info(os_type) of
        {unix,BSD} when BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd -> 104;
        {unix,_} -> 108
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
