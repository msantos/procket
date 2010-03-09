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

-export([init/0,open/1,poll/1,close/2,listen/1,listen/2]).
-export([make_args/2,progname/0]).

-on_load(on_load/0).


init() ->
    on_load().

on_load() ->
    erlang:load_nif(progname(), []).

open(_) ->
    erlang:error(not_implemented).

poll(_) ->
    erlang:error(not_implemented).

close(_,_) ->
    erlang:error(not_implemented).

listen(Port) ->
    listen(Port, []).
listen(Port, Options) when is_integer(Port), is_list(Options) ->
    Opt = case proplists:lookup(pipe, Options) of
        none -> Options ++ [{pipe, mktmp:dir() ++ "/sock"}];
        _ -> Options
    end,
    {ok, Sockfd} = open(proplists:get_value(pipe, Opt)),
    Cmd = make_args(Port, Opt),
    case os:cmd(Cmd) of
        [] ->
            FD = poll(Sockfd),
            close(proplists:get_value(pipe, Opt), Sockfd),
            FD;
        Error ->
            {error, {procket_cmd, Error}}
    end.

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
                protocol
            ], proplists:lookup(Arg, Options) /= none ],
        " ") ++ Bind.

get_switch({pipe, Arg})         -> "-p " ++ Arg;
get_switch({protocol, raw})     -> "-P 0";
get_switch({protocol, icmp})    -> "-P 1";
get_switch({protocol, tcp})     -> "-P 6";
get_switch({protocol, udp})     -> "-P 17";
get_switch({ip, Arg}) when is_tuple(Arg) -> inet_parse:ntoa(Arg);
get_switch({ip, Arg}) when is_list(Arg) -> Arg.

progname() ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        ?MODULE
    ]).


