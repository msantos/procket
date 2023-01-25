%%% @copyright 2011-2023 Michael Santos <michael.santos@gmail.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(bpf).

-include("bpf.hrl").

% BPF ioctl
-export([
    open/1,
    buf/1,
    hdr/1,
    packet/3,
    ctl/2, ctl/3
]).
% BPF filters
-export([
    insn/1,
    stmt/2,
    jump/4,
    offset/1
]).
% Utility functions
-export([
    pad/1,
    align/1,
    io/2,
    iow/3,
    ior/3,
    iowr/3,
    ioc/4,
    sizeof/1,
    alignment/0
]).

open(Dev) ->
    {ok, Socket} = procket:dev("bpf"),

    try init(Socket, Dev) of
        {ok, Len} ->
            {ok, Socket, Len}
    catch
        error:_ ->
            procket:close(Socket),
            {error, enxio}
    end.

%%
%% Get bpf attributes
%%
ctl(Socket, blen) ->
    case procket:ioctl(Socket, ?BIOCGBLEN, <<1:32/native>>) of
        {ok, Len} -> {ok, procket:ntohl(Len)};
        Error -> Error
    end;
ctl(Socket, dlt) ->
    case procket:ioctl(Socket, ?BIOCGDLT, <<1:32/native>>) of
        {ok, DLT} -> {ok, procket:ntohl(DLT)};
        Error -> Error
    end;
% struct bpf_dtlist
ctl(Socket, dltlist) ->
    {ok, DLTs, [Res]} = procket:alloc([
        <<32:4/native-unsigned-integer-unit:8>>,
        {ptr, 32 * 4},
        % pad, needed?
        <<0:32>>
    ]),
    case procket:ioctl(Socket, ?BIOCGDLTLIST, DLTs) of
        {ok, _} ->
            {ok, List} = procket:buf(Res),
            Endian = erlang:system_info(endian),
            {ok, [binary:decode_unsigned(<<N:32>>, Endian) || <<N:32>> <= List, N /= 0]};
        Error ->
            Error
    end;
ctl(Socket, flush) ->
    procket:ioctl(Socket, ?BIOCFLUSH, <<0:32/native>>);
% struct ifreq
ctl(Socket, getif) ->
    case procket:ioctl(Socket, ?BIOCGETIF, <<0:32/integer-unit:8>>) of
        {ok, Ifname} ->
            {ok, binary_to_list(hd(binary:split(Ifname, <<0>>)))};
        Error ->
            Error
    end;
ctl(Socket, hdrcmplt) ->
    case procket:ioctl(Socket, ?BIOCGHDRCMPLT, <<1:32/native>>) of
        {ok, Bool} ->
            {ok, bool(Bool)};
        Error ->
            Error
    end;
ctl(Socket, seesent) ->
    case procket:ioctl(Socket, ?BIOCGSEESENT, <<1:32/native>>) of
        {ok, Bool} ->
            {ok, bool(Bool)};
        Error ->
            Error
    end;
%% struct bpf_stat {
%%     u_int bs_recv;
%%     u_int bs_drop;
%% };
ctl(Socket, stats) ->
    case procket:ioctl(Socket, ?BIOCGSTATS, <<0:32, 0:32>>) of
        {ok, <<Recv:4/native-unsigned-integer-unit:8, Drop:4/native-unsigned-integer-unit:8>>} ->
            {ok, {Recv, Drop}};
        Error ->
            Error
    end;
%ctl(Socket, timeout) ->
%    Size = sizeof(timeval),
%    procket:ioctl(Socket, ?BIOCGRTIMEOUT, <<0:Size/bytes>>);

ctl(Socket, version) ->
    case procket:ioctl(Socket, ?BIOCVERSION, <<0:32>>) of
        {ok, <<Major:2/native-unsigned-integer-unit:8, Minor:2/native-unsigned-integer-unit:8>>} ->
            {Major, Minor};
        Error ->
            Error
    end;
ctl(_Socket, _Request) ->
    {error, not_supported}.

%%
%% Set bpf attributes
%%
ctl(Socket, blen, Len) ->
    case procket:ioctl(Socket, ?BIOCSBLEN, <<Len:32/native>>) of
        {ok, Len} -> {ok, procket:ntohl(Len)};
        Error -> Error
    end;
ctl(Socket, dlt, DLT) ->
    case procket:ioctl(Socket, ?BIOCSDLT, <<DLT:32/native>>) of
        {ok, N} -> {ok, procket:ntohl(N)};
        Error -> Error
    end;
ctl(Socket, hdrcmplt, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCSHDRCMPLT, bool(Bool));
ctl(Socket, immediate, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCIMMEDIATE, bool(Bool));
ctl(Socket, promisc, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCPROMISC, bool(Bool));
ctl(Socket, seesent, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCSSEESENT, bool(Bool));
ctl(Socket, setf, []) ->
    procket:ioctl(Socket, ?BIOCSETF, <<0:(?SIZEOF_STRUCT_BPF_PROGRAM * 8)>>);
ctl(Socket, setf, Insn) when is_list(Insn) ->
    % struct bpf_program
    {ok, Code, [Res]} = procket:alloc([
        <<(length(Insn)):4/native-unsigned-integer-unit:8>>,
        {ptr, list_to_binary(Insn)}
    ]),
    case procket:ioctl(Socket, ?BIOCSETF, Code) of
        {ok, _} ->
            procket:buf(Res);
        Error ->
            Error
    end;
ctl(Socket, setif, Ifname) ->
    % struct ifreq
    Ifreq = list_to_binary([
        Ifname,
        <<0:((15 * 8) - (length(Ifname) * 8)), 0:8>>,
        <<0:(16 * 8)>>
    ]),
    procket:ioctl(Socket, ?BIOCSETIF, Ifreq);
% struct timeval
%ctl(Socket, timeout, Timeout) ->
%    procket:ioctl(Socket, ?BIOCSRTIMEOUT, <<0:(sizeof(timeval))/bytes>>);

ctl(_Socket, _Request, _Arg) ->
    {error, not_supported}.

bool(true) -> <<1:32/native>>;
bool(false) -> <<0:32>>;
bool(<<1:32/native>>) -> true;
bool(<<0:32>>) -> false.

%% struct bpf_hdr {
%%     struct BPF_TIMEVAL bh_tstamp;   /* time stamp */
%%     bpf_u_int32 bh_caplen;  /* length of captured portion */
%%     bpf_u_int32 bh_datalen; /* original length of packet */
%%     u_short     bh_hdrlen;  /* length of bpf header (this struct
%%                                 plus alignment padding) */
%% };

%% struct BPF_TIMEVAL bh_tstamp;
%%
%% On 32-bit, struct timeval32: 4 bytes tv_sec, 4 bytes tv_usec
%% On 64-bit, struct timeval: 8 bytes tv_sec, 4 bytes tv_usec
%%
%% On NetBSD, the bh_tstamp is like the following.
%%      struct bpf_timeval {
%%              long tv_sec;
%%              long tv_usec;
%%      };
%% So,
%% On 32-bit, struct bpf_timeval: 4 bytes tv_sec, 4 bytes tv_usec
%% On 64-bit, struct bpf_timeval: 8 bytes tv_sec, 8 bytes tv_usec

pad(Len) ->
    align(Len) - Len.

align(N) ->
    (N + (?BPF_ALIGNMENT - 1)) band bnot (?BPF_ALIGNMENT - 1).

buf(Data) when is_binary(Data) ->
    buf_1(hdr(Data), Data).

buf_1({bpf_hdr, Time, Caplen, Datalen, Hdrlen}, Data) ->
    buf_2(Time, Datalen, packet(Hdrlen, Caplen, Data));
buf_1(Error, _) ->
    Error.

buf_2(Time, Datalen, {bpf_packet, Packet, Rest}) ->
    {bpf_buf, Time, Datalen, Packet, Rest};
buf_2(_Time, _Datalen, Error) ->
    Error.

hdr(Data) ->
    Size = erlang:system_info({wordsize, external}),
    UsecSize =
        case os:type() of
            {_, netbsd} ->
                Size;
            % OSX
            _ ->
                4
        end,
    case Data of
        <<Sec:Size/native-unsigned-integer-unit:8, Usec:UsecSize/native-unsigned-integer-unit:8,
            Caplen:4/native-unsigned-integer-unit:8, Datalen:4/native-unsigned-integer-unit:8,
            Hdrlen:2/native-unsigned-integer-unit:8, _/binary>> ->
            Time = {Sec div 1000000, Sec rem 1000000, Usec},
            {bpf_hdr, Time, Caplen, Datalen, Hdrlen};
        _ ->
            {error, bad_hdr}
    end.

packet(Hdrlen, Caplen, Data) ->
    % FIXME In some cases, 2 bytes of padding is lost or
    % FIXME dropped. For example, a packet of 174 bytes
    % FIXME should be padded to 176 bytes, but only 174
    % FIXME bytes is left in the buf.
    Len = Hdrlen + Caplen,
    Pad =
        case byte_size(Data) of
            Len -> 0;
            _ -> pad(Len)
        end,

    case Data of
        <<_Hdr:Hdrlen/bytes, Packet:Caplen/bytes, _Pad:Pad/bytes, Rest/binary>> ->
            {bpf_packet, Packet, Rest};
        _ ->
            {error, bad_packet}
    end.

%%-------------------------------------------------------------------------
%%% BPF filtering
%%-------------------------------------------------------------------------
insn(#insn{
    code = Code,
    jt = JT,
    jf = JF,
    k = K
}) ->
    <<Code:2/native-unsigned-integer-unit:8, JT:8, JF:8, K:4/native-unsigned-integer-unit:8>>;
insn(<<Code:2/native-unsigned-integer-unit:8, JT:8, JF:8, K:4/native-unsigned-integer-unit:8>>) ->
    #insn{
        code = Code,
        jt = JT,
        jf = JF,
        k = K
    }.

stmt(Code, K) when is_integer(Code), is_integer(K) ->
    insn(#insn{
        code = Code,
        k = K
    }).

jump(Code, K, JT, JF) when
    is_integer(Code),
    is_integer(K),
    is_integer(JT),
    is_integer(JF)
->
    insn(#insn{
        code = Code,
        jt = JT,
        jf = JF,
        k = K
    }).

offset(word) -> ?BPF_W;
offset(halfword) -> ?BPF_H;
offset(byte) -> ?BPF_B;
offset(?BPF_W) -> word;
offset(?BPF_H) -> halfword;
offset(?BPF_B) -> byte.

%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
%% BSD ioctl request calculation (taken from ioccom.h)
define(gseesent) ->
    proplists:get_value(
        os:type(),
        [
            {{unix, netbsd}, 120}
        ],
        118
    );
define(sseesent) ->
    proplists:get_value(
        os:type(),
        [
            {{unix, netbsd}, 121}
        ],
        119
    ).

ioc(Inout, Group, Name, Len) when is_atom(Name) ->
    ioc(Inout, Group, define(Name), Len);
ioc(Inout, Group, Num, Len) ->
    procket_ioctl:ioc(Inout, Group, Num, Len).

io(G, N) when is_atom(N) ->
    io(G, define(N));
io(G, N) ->
    procket_ioctl:io(G, N).

iow(G, N, T) when is_atom(N) ->
    iow(G, define(N), T);
iow(G, N, T) ->
    procket_ioctl:iow(G, N, T).

ior(G, N, T) when is_atom(N) ->
    ior(G, define(N), T);
ior(G, N, T) ->
    procket_ioctl:ior(G, N, T).

iowr(G, N, T) when is_atom(N) ->
    iowr(G, define(N), T);
iowr(G, N, T) ->
    procket_ioctl:iowr(G, N, T).

sizeof(timeval) ->
    erlang:system_info({wordsize, external}) + ?SIZEOF_U_INT;
sizeof(ifreq) ->
    case os:type() of
        {_, netbsd} ->
            144;
        % OSX
        _ ->
            32
    end.

alignment() ->
    case os:type() of
        {_, netbsd} ->
            erlang:system_info({wordsize, external});
        % OSX
        _ ->
            ?SIZEOF_INT32_T
    end.

init(Socket, Dev) ->
    % Set the interface for the bpf
    {ok, _} = ctl(Socket, setif, Dev),

    % Allow caller to provide packet header (header complete)
    {ok, _} = ctl(Socket, hdrcmplt, true),

    % Return packets sent from the interface
    {ok, _} = ctl(Socket, seesent, true),

    % Return packets immediately (do not wait until buffer full)
    {ok, _} = ctl(Socket, immediate, true),

    % Get bpf buf len
    ctl(Socket, blen).
