%% Copyright (c) 2010-2015, Vasu Dasari <vdasari@gmail.com>
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
%%

%% Setup following network namespace as follows:
%%
%% ip netns add blue
%% ip link add veth0 type veth peer name veth1
%% ip link set veth1 netns blue
%% ip netns exec blue ifconfig veth1 10.1.1.1/24 up
%% ifconfig veth0 10.1.1.100/24 up
%% ping 10.1.1.1 # This should be successful

%% Then execute the example as follows:
%%      sudo erl -pa _build/default/lib/procket/ebin/ -pa ebin -s sniff start

-module(sniff).
-export([start/0]).

start() ->
    {ok, Fd} = procket:open(0, [{namespace,"/var/run/netns/blue"},
            {protocol, 16#0008}, {type, raw}, {family, packet}]),
    ok = packet:bind(Fd, packet:ifindex(Fd,"veth1")),
    erlang:open_port({fd, Fd, Fd}, [binary, stream]),
    loop().

loop() ->
    receive
        Data ->
            filter(Data),
            loop()
    after
        5000 ->
            error_logger:info_msg("Loop timeout",[])
    end.

filter({_,{data,Data}}) ->
    error_logger:info_msg("Data ~p", [Data]),
    Data;
filter(Data) ->
    ok.
