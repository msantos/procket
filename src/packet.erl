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
-module(packet).
-export([
        socket/0,
        makesum/1,
        ifindex/2,
        promiscuous/2,
        send/3
    ]).

-define(SIOCGIFINDEX, 16#8933).
-define(PF_PACKET, 17).

% Options for promiscuous mode
-define(SOL_PACKET, 263).
-define(PACKET_ADD_MEMBERSHIP, 1).
-define(PACKET_DROP_MEMBERSHIP, 2).
-define(PACKET_MR_PROMISC, 1).

socket() ->
    procket:listen(0, [{protocol, 16#0008}, {type, raw}, {family, packet}]).

ifindex(Socket, Dev) ->
    {ok, <<_Ifname:16/bytes, Ifr:8, _/binary>>} = procket:ioctl(Socket,
        ?SIOCGIFINDEX,
        list_to_binary([
                Dev, <<0:((16*8) - (length(Dev)*8)), 0:128>>
            ])),
    Ifr.

send(S, Ifindex, Packet) ->
    procket:sendto(S, Packet, 0,
        <<
        ?PF_PACKET:16/native,   % sll_family: PF_PACKET
        0:16,                   % sll_protocol: Physical layer protocol
        Ifindex:32/native,  	% sll_ifindex: Interface number
        0:16,		    		% sll_hatype: Header type
        0:8,		    		% sll_pkttype: Packet type
        0:8,		    		% sll_halen: address length
        0:8,		    		% sll_addr[8]: physical layer address
        0:8,		    		% sll_addr[8]: physical layer address
        0:8,		    		% sll_addr[8]: physical layer address
        0:8,		    		% sll_addr[8]: physical layer address
        0:8,		    		% sll_addr[8]: physical layer address
        0:8,		    		% sll_addr[8]: physical layer address
        0:8,		    		% sll_addr[8]: physical layer address
        0:8			     	    % sll_addr[8]: physical layer address
        >>).

promiscuous(Socket, Ifindex) ->
    % struct packet_mreq
    procket:setsockopt(Socket, ?SOL_PACKET, ?PACKET_ADD_MEMBERSHIP, <<
        Ifindex:32/native,              % mr_ifindex: interface index
        ?PACKET_MR_PROMISC:16/native,   % mr_type: action
        0:16,                           % mr_alen: address length
        0:64                            % mr_address[8]:  physical layer address
        >>).

makesum(Hdr) -> 16#FFFF - checksum(Hdr).

checksum(Hdr) ->
    lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).

compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).


