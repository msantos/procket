%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
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
-module(bpf).

-include("bpf.hrl").

% BPF ioctl
-export([
        open/1,
        data/1,
        promiscuous/1,
        ctl/2, ctl/3
    ]).
% BPF filters
-export([
        insn/1,
        stmt/2, jump/4,
        offset/1
    ]).
% Utility functions
-export([
        pad/1, align/1,
        io/2, iow/3, ior/3, iowr/3, ioc/4,
        sizeof/1
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
%ctl(Socket, dltlist) ->
%    procket:ioctl(Socket, ?BIOCGDLTLIST, <<1:32/native>>);

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

%ctl(Socket, timeout) ->
%    Size = sizeof(timeval),
%    procket:ioctl(Socket, ?BIOCGRTIMEOUT, <<0:Size/bytes>>);

ctl(Socket, version) ->
    case procket:ioctl(Socket, ?BIOCVERSION, <<0:32>>) of
        {ok, <<Major:2/native-unsigned-integer-unit:8,
            Minor:2/native-unsigned-integer-unit:8>>} ->
            {Major, Minor};
        Error ->
            Error
    end.

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

ctl(Socket, setif, Ifname) ->
    % struct ifreq
    Ifreq = list_to_binary([
        Ifname, <<0:((15*8) - (length(Ifname)*8)), 0:8>>,
        <<0:(16*8)>>
    ]),
    procket:ioctl(Socket, ?BIOCSETIF, Ifreq);

ctl(Socket, immediate, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCIMMEDIATE, bool(Bool));

ctl(Socket, hdrcmplt, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCSHDRCMPLT, bool(Bool));

ctl(Socket, seesent, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCSSEESENT, bool(Bool));

ctl(Socket, setf, []) ->
    procket:ioctl(Socket, ?BIOCSETF, <<0:((?SIZEOF_STRUCT_BPF_PROGRAM)*8)>>);
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
    end.

% struct timeval
%ctl(Socket, timeout, Timeout) ->
%    procket:ioctl(Socket, ?BIOCSRTIMEOUT, <<0:(sizeof(timeval))/bytes>>).


bool(true) -> <<1:32/native>>;
bool(false) -> <<0:32>>.

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

%% #define BPF_ALIGNMENT sizeof(int32_t)
%% #define BPF_WORDALIGN(x) (((x)+(BPF_ALIGNMENT-1))&~(BPF_ALIGNMENT-1))
pad(Len) ->
    align(Len) - Len.

align(N) ->
    (N + (?BPF_ALIGNMENT-1)) band bnot (?BPF_ALIGNMENT-1).

data(Data) when is_binary(Data) ->
    Size = erlang:system_info({wordsize, external}),

    <<Sec:Size/native-unsigned-integer-unit:8,
      Usec:4/native-unsigned-integer-unit:8,
      Caplen:4/native-unsigned-integer-unit:8,
      Datalen:4/native-unsigned-integer-unit:8,
      Hdrlen:2/native-unsigned-integer-unit:8,
      _/binary>> = Data,

    Time = {Sec div 1000000, Sec rem 1000000, Usec},

    Pad = pad(Caplen),

    % Include the padding
    <<_Hdr:Hdrlen/bytes,
    Packet:Caplen/bytes,
    _Pad:Pad/bytes,
    Rest/binary>> = Data,

    {Time, Datalen, Packet, Rest}.


promiscuous(FD) ->
    procket:ioctl(FD, ?BIOCPROMISC, 0).


%%-------------------------------------------------------------------------
%%% BPF filtering
%%-------------------------------------------------------------------------
insn(#insn{
        code = Code,
        jt = JT,
        jf = JF,
        k = K
    }) ->
    <<Code:2/native-unsigned-integer-unit:8,
    JT:8, JF:8,
    K:4/native-unsigned-integer-unit:8>>;
insn(<<Code:2/native-unsigned-integer-unit:8,
    JT:8, JF:8,
    K:4/native-unsigned-integer-unit:8>>) ->
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

jump(Code, K, JT, JF) when is_integer(Code), is_integer(K),
    is_integer(JT), is_integer(JF) ->
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
ioc(Inout, Group, Num, Len) ->
    Inout bor ((Len band ?IOCPARM_MASK) bsl 16) bor (Group bsl 8) bor Num.

io(G,N) ->
    ioc(?IOC_VOID, G, N, 0).

iow(G,N,T) ->
    ioc(?IOC_IN, G, N, T).

ior(G,N,T) ->
    ioc(?IOC_OUT, G, N, T).

iowr(G,N,T) ->
    ioc(?IOC_INOUT, G, N, T).

sizeof(timeval) ->
    erlang:system_info({wordsize, external}) + ?SIZEOF_U_INT.

init(Socket, Dev) ->
    % Set the interface for the bpf
    {ok, _} = ctl(Socket, setif, Dev),

    % Allow caller to provide packet header (header complete)
    {ok, _} = ctl(Socket, hdrcmplt, true),

    % Return packets sent from the interface
    {ok, _} = ctl(Socket, seesent, true),

    % Get bpf buf len
    ctl(Socket, blen).
