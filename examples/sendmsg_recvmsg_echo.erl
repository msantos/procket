%% Copyright (c) 2015, Andrew Thompson <andrew@hijacked.us>
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
-module(sendmsg_recvmsg_echo).

-export([start/0]).

-define(PORT, 2097).

start() ->
    {ok, FD} = procket:socket(inet6, dgram, udp),
    %% set IPV6_RECVPKTINFO
    RecvPktInfo = case os:type() of
        {unix, linux} -> 49;
        {unix, _} -> 36
    end,
    ok = procket:setsockopt(FD, 41, RecvPktInfo, <<1:32>>),
    Family = procket:family(inet6),
    io:format("inet family ~p~n", [Family]),
    %% bind to :: on port ?PORT
    SA = list_to_binary([procket:sockaddr_common(Family, 28), <<?PORT:16/integer-unsigned-big, 0:192>>]),
    ok = procket:bind(FD, SA),
    loop(FD).

loop(FD) ->
    %% recv a packet up to 512 bytes long, along with 512 bytes of control
    %% data
    case procket:recvmsg(FD, 512, 512, 0) of
        {error, eagain} ->
            loop(FD);
        {ok, Buf, From0, CtrlData, Flags} ->
            io:format("Buffer ~p, From ~p, Ctrldata ~p Flags ~p~n", [Buf,
                                                                     From0,
                                                                     CtrlData,
                                                                     Flags]),
            From = fixup_from(From0),
            %% echo the packet back, but set the destination address to the
            %% 'from' of the previous packet, and send the previous message's
            %% control data so that the source address is set to the
            %% destination address of the previous packet
            ok = procket:sendmsg(FD, Buf, 0, From, fixup_control_data(CtrlData)),
            loop(FD)
    end.

%% on the BSDs, use the 'length' field in the sockaddr_storage field to
%% trim it to length. FreeBSD throws einval if you pass overlong data as the
%% destination address to sendmsg.
fixup_from(From0) ->
    case erlang:system_info(os_type) of
        {unix,BSD} when BSD == darwin;
                BSD == openbsd;
                BSD == netbsd;
                BSD == freebsd ->
            <<Length:8/integer-unsigned,_/binary>> = From0,
            binary:part(From0, 0, Length);
        {unix,_} ->
            From0
    end.

%% on freebsd, the PKTINFO control data that comes back from recvmsg
%% is 4 bytes too long. I don't know why.
fixup_control_data(CData) ->
    lists:map(fun({41, 46, In6Pktinfo}) ->
                {41, 46, binary:part(In6Pktinfo, 0, 20)};
            (E) -> E
        end, CData).

