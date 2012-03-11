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

%%
%% Functions for dealing with PF_PACKET sockets and other Linux
%% specific interfaces.
%%
-module(packet).
-include("packet.hrl").
-export([
        socket/0, socket/1,
        iflist/0,
        makesum/1,
        arplookup/1,
        gateway/0, gateway/1, gateway_addr/1,
        default_interface/0,
        ifindex/2,
        ipv4address/2,
        macaddress/2,
        promiscuous/2,
        bind/2,
        bindtodevice/2,
        filter/2, unfilter/1, unfilter/2,
        send/3
    ]).


%%-------------------------------------------------------------------------
%% Convenience function to return a raw socket
%%-------------------------------------------------------------------------
socket() ->
    socket(?ETH_P_IP).
socket(Protocol) when is_integer(Protocol) ->
    procket:open(0, [
            {protocol, procket:ntohs(Protocol)},
            {type, raw},
            {family, packet}
        ]).


%%-------------------------------------------------------------------------
%% Lookup the MAC address of an IP
%%-------------------------------------------------------------------------
arplookup({A1,A2,A3,A4}) ->
    arplookup(inet_parse:ntoa({A1,A2,A3,A4}));
arplookup(IPaddr) when is_list(IPaddr) ->
    {ok, FH} = file:open("/proc/net/arp", [read,raw]),
    MAC = arplookup_iter(FH, IPaddr),
    file:close(FH),
    MAC.

arplookup_iter(FH, IPaddr) ->
    arplookup_iter_1(FH, IPaddr, file:read_line(FH)).

arplookup_iter_1(FH, IPaddr, {ok, Line}) ->
    case string:tokens(Line, "\s\n") of
        [IPaddr, _HWType, _Flags, MAC|_] ->
            list_to_tuple([ erlang:list_to_integer(E, 16) ||
                E <- string:tokens(MAC, ":") ]);
        _ ->
            arplookup_iter(FH, IPaddr)
    end;
arplookup_iter_1(_FH, _IPaddr, eof) ->
    false.


%%-------------------------------------------------------------------------
%% Return the MAC and IP address of the gateway for an interface. If an
%% interface is not specified, the first gateway found is returned.
%%-------------------------------------------------------------------------
gateway() ->
    gateway([]).
gateway(Dev) ->
    gateway_res(gateway_addr(Dev)).

gateway_res(false) -> false;
gateway_res(IP) -> gateway_res_1(arplookup(IP), IP).
gateway_res_1(false, _) -> false;
gateway_res_1(MAC, IP) -> {ok, MAC, IP}.

gateway_addr(Dev) ->
    {ok, FH} = file:open("/proc/net/route", [read,raw]),
    IP = gateway_addr_iter(FH, Dev),
    file:close(FH),
    IP.

gateway_addr_iter(FH, Dev) ->
    gateway_addr_iter_1(FH, Dev, file:read_line(FH)).

gateway_addr_iter_1(FH, Dev, {ok, Line}) ->
    case string:tokens(Line, "\t") of
        [Dev, "00000000", IP, "0003"|_] ->
            gateway_addr_res(IP);
        [_, "00000000", IP, "0003"|_] when Dev == [] ->
            gateway_addr_res(IP);
        _ ->
            gateway_addr_iter(FH, Dev)
    end;
gateway_addr_iter_1(_FH, _Dev, eof) ->
    false.

gateway_addr_res(IPHex) ->
    {ok,[Addr],[]} = io_lib:fread("~16u", IPHex),
    <<A1,A2,A3,A4>> = <<Addr:32/native>>,
    {A1,A2,A3,A4}.


%%-------------------------------------------------------------------------
%% Return the default network interface as a list. For most systems,
%% a single element list will be returned. On systems without a default
%% gateway or with multiple defaults, an empty list or a list with more
%% than 1 element will be returned.
%%-------------------------------------------------------------------------
default_interface() ->
    [ If || If <- iflist(), gateway(If) /= false ].


%%-------------------------------------------------------------------------
%% List of network interfaces.
%%-------------------------------------------------------------------------
iflist() ->
    {ok, Ifs} = inet:getiflist(),
    Ifs.


%%-------------------------------------------------------------------------
%% The interface index associated with the network device. Required
%% for packet:send/3.
%%-------------------------------------------------------------------------
ifindex(Socket, Dev) ->
    {ok, <<_Ifname:16/bytes, Ifr:8, _/binary>>} = procket:ioctl(Socket,
        ?SIOCGIFINDEX,
        list_to_binary([
                Dev, <<0:((15*8) - (length(Dev)*8)), 0:8, 0:128>>
            ])),
    Ifr.


%%-------------------------------------------------------------------------
%% procket:sendto/4 with defaults set
%%-------------------------------------------------------------------------
send(Socket, Ifindex, Packet) ->
    procket:sendto(Socket, Packet, 0,
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


%%-------------------------------------------------------------------------
%% Enable promiscuous mode on a socket
%%-------------------------------------------------------------------------
promiscuous(Socket, Ifindex) ->
    % struct packet_mreq
    procket:setsockopt(Socket, ?SOL_PACKET, ?PACKET_ADD_MEMBERSHIP, <<
        Ifindex:32/native,              % mr_ifindex: interface index
        ?PACKET_MR_PROMISC:16/native,   % mr_type: action
        0:16,                           % mr_alen: address length
        0:64                            % mr_address[8]:  physical layer address
        >>).


%%-------------------------------------------------------------------------
%% Bind a PF_PACKET socket to an interface.
%%-------------------------------------------------------------------------
bind(Socket, Ifindex) ->
    Sockaddr_ll = <<
        ?PF_PACKET:16/native,   % sll_family: PF_PACKET
        0:16,                   % sll_protocol: Physical layer protocol
        Ifindex:32/native,      % sll_ifindex: Interface number
        0:16,                   % sll_hatype: Header type
        0:8,                    % sll_pkttype: Packet type
        0:8,                    % sll_halen: address length
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8,                    % sll_addr[8]: physical layer address
        0:8                     % sll_addr[8]: physical layer address
    >>,

    procket:bind(Socket, Sockaddr_ll).


%%-------------------------------------------------------------------------
%% Bind socket to interface. Equivalent to the {interface, Device} option
%% but requires running Erlang with heightened privs (CAP_NET_RAW)
%%-------------------------------------------------------------------------
bindtodevice(Socket, Dev) when length(Dev) < 16 ->
    % struct ifreq
    procket:setsockopt(Socket, ?SOL_SOCKET, ?SO_BINDTODEVICE, list_to_binary([
        Dev, <<0:((15*8) - (length(Dev)*8)), 0:8>>, % ifrn_name[IFNAMSIZ]: interface name
        <<0:(16*8)>>
    ])).


%%-------------------------------------------------------------------------
%% Retrieve the IPv4 address of the interface.  Equivalent to
%% inet:ifget(Dev, [addr]).
%%-------------------------------------------------------------------------
ipv4address(Socket, Dev) ->
    % struct ifreq, struct sockaddr_in
    {ok, <<_Ifname:16/bytes,
        ?PF_INET:16/native, % sin_family
        _:16,               % sin_port
        SA1,SA2,SA3,SA4,    % sin_addr
        _/binary>>} = procket:ioctl(Socket,
        ?SIOCGIFADDR,
        list_to_binary([
                Dev, <<0:((15*8) - (length(Dev)*8)), 0:8>>,
                % struct sockaddr
                <<?PF_INET:16/native,       % family
                0:112>>
            ])),
    {SA1,SA2,SA3,SA4}.


%%-------------------------------------------------------------------------
%% Retrieve the MAC address of the interface.  Equivalent to
%% inet:ifget(Dev, [hwaddr]).
%%-------------------------------------------------------------------------
macaddress(Socket, Dev) ->
    {ok, <<_Ifname:16/bytes,
        _:16,                       % family
        SM1,SM2,SM3,SM4,SM5,SM6,    % mac address
        _/binary>>} = procket:ioctl(Socket,
        ?SIOCGIFHWADDR,
        list_to_binary([
                Dev, <<0:((15 - length(Dev))*8), 0:8, 0:128>>
            ])),
    {SM1,SM2,SM3,SM4,SM5,SM6}.

%%-------------------------------------------------------------------------
%% Berkely Packet Filter
%%
%% Filters can be applied in bpf(4) format to any socket.
%%
%% Note: BPF uses 4 bytes for the instruction length, LSF uses 2 bytes
%% but has 2 bytes pad
%%
%% See:
%%  http://www.kernel.org/doc/Documentation/networking/filter.txt
%%
%%-------------------------------------------------------------------------
filter(Socket, Insn) when is_list(Insn) ->
    filter_1(Socket, Insn, ?SO_ATTACH_FILTER).

% Remove or replace filter
unfilter(Socket) ->
    Size = erlang:system_info(wordsize),
    Pad = procket:wordalign(2),
    procket:setsockopt(Socket, ?SOL_SOCKET, ?SO_DETACH_FILTER,
        <<0,0, 0:(Pad*8), 0:(Size*8)>>).
unfilter(Socket, Insn) when is_list(Insn) ->
    filter_1(Socket, Insn, ?SO_DETACH_FILTER).

filter_1(Socket, Insn, Optname) ->
    Pad = procket:wordalign(2),
    {ok, Fcode, [Res]} = procket:alloc([
        <<(length(Insn)):2/native-unsigned-integer-unit:8, 0:(Pad*8)>>,
        {ptr, list_to_binary(Insn)}
    ]),
    case procket:setsockopt(Socket, ?SOL_SOCKET, Optname, Fcode) of
        ok ->
            procket:buf(Res);
        Error ->
            Error
    end.


%%-------------------------------------------------------------------------
%% Utility functions, copied from epcap
%%-------------------------------------------------------------------------
makesum(Hdr) -> 16#FFFF - checksum(Hdr).

checksum(Hdr) ->
    lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).

compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).
