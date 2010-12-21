%% Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
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
        socket/3,
        listen/1,listen/2,
        connect/2,
        accept/1,accept/2,
        close/1,
        recv/2,recvfrom/2,recvfrom/4,
        sendto/4,
        bind/2,
        ioctl/3,
        setsockopt/4
    ]).
-export([make_args/2,progname/0]).

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

socket(Family, Type, Protocol) when is_atom(Family) ->
    socket(family(Family), Type, Protocol);
socket(Family, Type, Protocol) when is_atom(Type) ->
    socket(Family, type(Type), Protocol);
socket(Family, Type, Protocol) when is_atom(Protocol) ->
    socket(Family, Type, protocol(Protocol));
socket(_,_,_) ->
    erlang:error(not_implemented).

ioctl(_,_,_) ->
    erlang:error(not_implemented).

sendto(_,_,_,_) ->
    erlang:error(not_implemented).

setsockopt(_,_,_,_) ->
    erlang:error(not_implemented).


open(Port) ->
    open(Port, []).
open(Port, Options) when is_integer(Port), is_list(Options) ->
    Opt = case proplists:get_value(pipe, Options) of
        undefined ->
            Tmp = mktmp:dirname(),
            ok = mktmp:make_dir(Tmp),
            Path = Tmp ++ "/sock",
            [{pipe, Path}, {tmpdir, Tmp}] ++ Options;
        _ ->
            [{tmpdir, false}] ++ Options
    end,
    open1(Port, Opt).

open1(Port, Options) ->
    Pipe = proplists:get_value(pipe, Options),
    {ok, Sockfd} = fdopen(Pipe),
    Cmd = make_args(Port, Options),
    case os:cmd(Cmd) of
        [] ->
            FD = fdget(Sockfd),
            cleanup(Sockfd, Pipe, Options),
            FD;
        Error ->
            cleanup(Sockfd, Pipe, Options),
            {error, {procket_cmd, Error}}
    end.

cleanup(Sockfd, Pipe, Options) ->
    close(Sockfd),
    ok = file:delete(Pipe),
    case proplists:get_value(tmpdir, Options) of
        false ->
            ok;
        Path ->
            mktmp:close(Path)
    end.

fdopen(Path) when is_list(Path) ->
    fdopen(list_to_binary(Path));
fdopen(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = socket(?PF_LOCAL, ?SOCK_STREAM, 0),
    Sun = <<?PF_LOCAL:16/native, Path/binary, 0:((?UNIX_PATH_MAX-byte_size(Path))*8)>>,
    ok = bind(Socket, Sun),
    ok = listen(Socket, ?BACKLOG),
    {ok, Socket}.

fdget(Socket) ->
    {ok, S} = accept(Socket),
    fdrecv(S).

make_args(Port, Options) ->
    Bind = " " ++ case proplists:lookup(ip, Options) of
        none ->
            integer_to_list(Port);
        IP ->
            get_switch(IP) ++ ":" ++ integer_to_list(Port)
    end,
    proplists:get_value(progname, Options, "sudo " ++ progname()) ++ " " ++
    string:join([ get_switch(proplists:lookup(Arg, Options)) || Arg <- [
                pipe,
                protocol,
                family,
                type,
                interface
            ], proplists:lookup(Arg, Options) /= none ],
        " ") ++ Bind.

get_switch({pipe, Arg}) ->
    "-p " ++ Arg;

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

get_switch({interface, Name}) when is_list(Name) ->
    % An interface name is expected to consist of a reasonable
    % subset of all charactes, use a whitelist and extend it if needed
    SName = [C || C <- Name, ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z))
                          or ((C >= $0) and (C =< $9)) or (C == $.)],
    "-I " ++ SName.

progname() ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        ?MODULE
    ]).


%% Protocol family (aka domain)
family(unspec) -> 0;
family(inet) -> 2;
family(packet) -> 17;
family(Proto) when Proto == local; Proto == unix; Proto == file -> 1;

family(0) -> unspec;
family(1) -> unix;
family(2) -> inet;
family(17) -> packet.


%% Socket type
type(stream) -> 1;
type(dgram) -> 2;
type(raw) -> 3;

type(1) -> stream;
type(2) -> dgram;
type(3) -> raw.


% Select a protocol within the family (0 means use the default
% protocol in the family)
protocol(raw) -> 0;
protocol(icmp) -> 1;
protocol(tcp) -> 6;
protocol(udp) -> 17;

protocol(0) -> raw;
protocol(1) -> icmp;
protocol(6) -> tcp;
protocol(17) -> udp.


