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

%% procket test client
-module(echo).

-export([start/0,start/1,start/2]).

-define(PORT, 54).

start() ->
    start(tcp).
start(tcp) ->
    start(?PORT, [{protocol, tcp}, {family, inet}, {type, stream}]);
start(udp) ->
    start(?PORT, [{protocol, udp}, {family, inet}, {type, dgram}]).
start(Port, Options) ->
    Protocol = proplists:get_value(protocol, Options, tcp),
    Family = proplists:get_value(family, Options, inet),
    {ok, Fd} = procket:open(Port, Options),
    io:format("Listening on: ~p/~p~n", [Port, Protocol]),
    listen(Protocol, Family, Fd, Port).

listen(tcp, Family, Fd, Port) ->
    {ok, S} = gen_tcp:listen(Port, [binary, Family, {fd, Fd}]),
    accept(S);
listen(udp, Family, Fd, Port) ->
    {ok, S} = gen_udp:open(Port, [binary, Family, {fd, Fd}]),
    recv(S).

accept(LS) ->
    {ok, S} = gen_tcp:accept(LS),
    spawn(fun() -> accept(LS) end),
    recv(S).

recv(S) ->
    receive
        {tcp, S, Data} ->
            gen_tcp:send(S, Data),
            recv(S);
        {tcp_closed, S} ->
            ok;
        {udp, S, IP, Port, Data} ->
            gen_udp:send(S, IP, Port, Data),
            recv(S);
        Error ->
            error_logger:error_report({recv, Error})
    end.
