%% icmp6.erl: an example of ICMPv6 support, modified from icmp.erl

%% Copyright (c) 2012 Kenji Rikitake <kenji.rikitake@acm.org>
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
-module(icmp6).
-export([ping/1, ping/2]).

-record(icmp, {
        valid,
        type, code, checksum,
        id, sequence,
        gateway,
        un,
        mtu
    }).

-record(state, {
        s,          % socket
        id,         % ping ID
        seq = 0,    % ping sequence number
        ip,         % IP Address
        family,     % IP Address Family
        protocol,   % ICMP protocol number
        n           % Number of pings
    }).

-define(ICMP_ECHO_REPLY, 0).
-define(ICMP_ECHO, 8).
-define(ICMPV6_ECHO_REPLY, 129).
-define(ICMPV6_ECHO, 128).

ping(IP) ->
    ping(IP, 1).
ping(IP, N) ->
    crypto:start(),
    Id = crypto:rand_uniform(0, 16#FFFF),
    {Family, Protocol, Addr} =
	case (catch inet:getaddr(IP, inet6)) of
	    {ok, V6Addr} ->
		{inet6, 'ipv6-icmp', V6Addr};
	    {error, _Else} ->
		{inet, icmp, IP}
	end,
    {ok, FD} = procket:open(0, [{protocol, Protocol}, {type, raw}, {family, Family}]),
    {ok, S} = gen_udp:open(0, [binary, {fd, FD}, Family]),
    loop(#state{
            s = S,
            id = Id,
            ip = Addr,
	    family = Family,
	    protocol = Protocol,
            n = N
        }).

loop(#state{n = N, seq = Seq}) when Seq >= N ->
    ok;
loop(#state{s = S, id = Id, seq = Seq, ip = IP,
	    family = Family, protocol = Protocol, n = N} = State) ->
    Packet = make_packet(Id, Seq, Family),
    % io:format("S = ~p, IP = ~p, Family = ~p, Protocol = ~p~n", [S, IP, Family, Protocol]),
    ok = gen_udp:send(S, IP, 0, Packet),
    Skip = case Family of
	inet6 -> 0;
	_inet -> 20
    end,
    receive
	{udp, S, _IP, _Port, <<_SD:Skip/bytes, Data/binary>>} ->
	    % io:format("_IP=~p, _Port=~p, _SD = ~p, Data = ~p~n", [_IP, _Port, _SD, Data]),
	    {ICMP, <<Mega:32/integer, Sec:32/integer, Micro:32/integer, Payload/binary>>} = icmp(Data),
	    error_logger:info_report([
				      {icmp_protocol, Protocol},
				      {type, ICMP#icmp.type},
				      {code, ICMP#icmp.code},
				      {checksum, ICMP#icmp.checksum},
				      {id, ICMP#icmp.id},
				      {sequence, ICMP#icmp.sequence},
				      {payload, Payload},
				      {time, timer:now_diff(erlang:now(), {Mega, Sec, Micro})}
				     ]),
	    sleep(N, Seq)
    after
	5000 ->
	    error_logger:error_report([{noresponse, Packet}])
    end,
    loop(State#state{seq = Seq + 1}).

make_packet(Id, Seq, Family) ->
    {Mega,Sec,USec} = erlang:now(),

    % Pad packet to 72 bytes
    Payload = list_to_binary(lists:seq($\s, $S)),

    Type = case Family of
	       inet6 ->
		   ?ICMPV6_ECHO;
	       _inet ->
		   ?ICMP_ECHO
	   end,

    CS = makesum(<<Type:8/integer-unsigned-big, 0:8, 0:16,
		   Id:16, Seq:16, Mega:32, Sec:32, USec:32, Payload/binary>>),
    <<
      Type:8/integer-unsigned-big, % Type
      0:8,    % Code
      CS:16,  % Checksum
      Id:16,  % Id
      Seq:16, % Sequence

      Mega:32, Sec:32, USec:32,   % Payload: time
      Payload/binary
    >>.

makesum(Hdr) -> 16#FFFF - checksum(Hdr).

checksum(Hdr) ->
    lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).

compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).

%% icmp(<<?ICMPV6_ECHO_REPLY:8/integer-unsigned-big, 0:8, Checksum:16,
%%      Id:16, Sequence:16, Payload/binary>>, 'ipv6-icmp') ->
%%     {#icmp{
%%             type = ?ICMPV6_ECHO_REPLY, code = 0, checksum = Checksum, id = Id,
%%             sequence = Sequence
%%        }, Payload};
%% icmp(<<?ICMP_ECHO_REPLY:8/integer-unsigned-big, 0:8, Checksum:16, Id:16, Sequence:16, Payload/binary>>, icmp) ->
%%     {#icmp{
%%             type = ?ICMP_ECHO_REPLY, code = 0, checksum = Checksum, id = Id,
%%             sequence = Sequence
%%        }, Payload}.

icmp(<<Type:8, Code:8, Checksum:16, Id:16, Sequence:16, Payload/binary>>) ->
    {#icmp{
            type = Type, code = Code, checksum = Checksum, id = Id,
            sequence = Sequence
       }, Payload}.

sleep(N,S) when N =:= S + 1 -> ok;
sleep(_,_) -> timer:sleep(1000).
