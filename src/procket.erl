%% Copyright (c) 2010-2012, Michael Santos <michael.santos@gmail.com>
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
-include_lib("kernel/include/file.hrl").

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
% for debugging
-export([
    get_progname/2,
    make_cli_args/1,
    progname/0
    ]).

-on_load(on_load/0).


init() ->
    on_load().

on_load() ->
    erlang:load_nif(progname(), []).


%%--------------------------------------------------------------------
%%% NIF Stubs
%%--------------------------------------------------------------------
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

write(FD, Buf) when is_binary(Buf) ->
    write_nif(FD, Buf);
write(FD, Buf) when is_list(Buf) ->
    writev(FD, Buf).

write_nif(_,_) ->
    erlang:error(not_implemented).

writev(_,_) ->
    erlang:error(not_implemented).

setsockopt(_,_,_,_) ->
    erlang:error(not_implemented).

getsockname(_,_) ->
    erlang:error(not_implemented).

errno_id(_) ->
    erlang:error(not_implemented).


%%--------------------------------------------------------------------
%%% Setuid helper
%%--------------------------------------------------------------------
dev(Dev) when is_list(Dev) ->
    open(0, [{dev, Dev}]).

open(Port) ->
    open(Port, []).
open(Port, Options) when is_integer(Port), is_list(Options) ->
    Progname = get_progname(progname(), Options),
    {Tmpdir, Pipe} = make_unix_socket_path(Options),
    Cmd = make_cli_args([
                {progname, Progname},
                {port, Port},
                {pipe, Pipe}
                ] ++ Options),

    Result = case fdopen(Pipe) of
        {ok, FD} ->
            Socket = exec(FD, Cmd),
            close(FD),
            Socket;
        Error ->
            Error
    end,

    cleanup_unix_socket(Tmpdir, Pipe),
    Result.

% Figure out how the procket helper should be called.
get_progname(Progname, Options) ->
    get_progname(progname, Progname, Options).

% Caller has passed in a path?
get_progname(progname, Default, Options) ->
    case proplists:get_value(progname, Options) of
        undefined ->
            get_progname(setuid, Default, Options);
        Progname ->
            Progname
    end;

% Is the default executable setuid/setgid?
get_progname(setuid, Progname, Options) ->
    case file:read_file_info(Progname) of
        {ok, #file_info{mode = Mode}} ->
            if
                % setuid
                Mode band 16#800 =:= 16#800 ->
                    Progname;
                % setgid
                Mode band 16#400 =:= 16#400 ->
                    Progname;
                true ->
                    get_progname(dev, Progname, Options)
            end
    end;

% Device requested and accessible?
get_progname(dev, Progname, Options) ->
    case proplists:get_value(dev, Options) of
        undefined ->
            get_progname(sudo, Progname, Options);
        Dev ->
            case file:read_file_info(Dev) of
                {ok, #file_info{access = read_write}} ->
                    Progname;
                {ok, _} ->
                    get_progname(sudo, Progname, Options);
                _Error ->
                    get_progname(sudo, Progname, Options)
            end
    end;

% Fall back to sudo
get_progname(sudo, Progname, _Options) ->
    "sudo " ++ Progname.

% Run the setuid helper
exec(FD, Cmd) ->
    case os:cmd(Cmd) of
        "0" ->
            fdget(FD);
        Error ->
            {error, errno_id(list_to_integer(Error))}
    end.

% Unix socket handling: retrieves the fd from the setuid helper
make_unix_socket_path(Options) ->
    {Tmpdir, Socket} = case proplists:get_value(pipe, Options) of
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
    file:delete(Pipe);
cleanup_unix_socket(Tmpdir, Pipe) ->
    file:delete(Pipe),
    procket_mktmp:close(Tmpdir).

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

% Construct the cli arguments for the helper
make_cli_args(Options) ->
    {[[{progname,Progname}|_],
      IPaddrs], Rest} = proplists:split(Options, [progname, ip]),
    IP = case IPaddrs of
        [] -> [];
        [Addr|_] -> Addr
    end,
    Args = Rest ++ [IP],
    Progname ++ " " ++
    string:join([ get_switch(Arg) || Arg <- Args ], " ") ++
    " > /dev/null 2>&1; printf $?".

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
        {error, bad_name} -> progname_ebin();
        _ -> progname_priv()
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
