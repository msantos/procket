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
        sizeof/1,
        dlt/1
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
        {ptr, 32*4},
        <<0:32>> % pad, needed?
    ]),
    case procket:ioctl(Socket, ?BIOCGDLTLIST, DLTs) of
        {ok, _} ->
            {ok, List} = procket:buf(Res),
            Endian = erlang:system_info(endian),
            {ok, [ binary:decode_unsigned(<<N:32>>, Endian) || <<N:32>> <= List, N /= 0 ]};
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


dlt(?DLT_NULL) -> null;
dlt(?DLT_EN10MB) -> en10mb;
dlt(?DLT_EN3MB) -> en3mb;
dlt(?DLT_AX25) -> ax25;
dlt(?DLT_PRONET) -> pronet;
dlt(?DLT_CHAOS) -> chaos;
dlt(?DLT_IEEE802) -> ieee802;
dlt(?DLT_ARCNET) -> arcnet;
dlt(?DLT_SLIP) -> slip;
dlt(?DLT_PPP) -> ppp;
dlt(?DLT_FDDI) -> fddi;
dlt(?DLT_ATM_RFC1483) -> atm_rfc1483;
dlt(?DLT_RAW) -> raw;
dlt(?DLT_SLIP_BSDOS) -> slip_bsdos;
dlt(?DLT_PPP_BSDOS) -> ppp_bsdos;
dlt(?DLT_PFSYNC) -> pfsync;
dlt(?DLT_ATM_CLIP) -> atm_clip;
dlt(?DLT_PPP_SERIAL) -> ppp_serial;
dlt(?DLT_C_HDLC) -> c_hdlc;
dlt(?DLT_CHDLC) -> chdlc;
dlt(?DLT_IEEE802_11) -> ieee802_11;
dlt(?DLT_LOOP) -> loop;
dlt(?DLT_LINUX_SLL) -> linux_sll;
dlt(?DLT_PFLOG) -> pflog;
dlt(?DLT_IEEE802_11_RADIO) -> ieee802_11_radio;
dlt(?DLT_APPLE_IP_OVER_IEEE1394) -> apple_ip_over_ieee1394;
dlt(?DLT_IEEE802_11_RADIO_AVS) -> ieee802_11_radio_avs;

dlt(null) -> ?DLT_NULL;
dlt(en10mb) -> ?DLT_EN10MB;
dlt(en3mb) -> ?DLT_EN3MB;
dlt(ax25) -> ?DLT_AX25;
dlt(pronet) -> ?DLT_PRONET;
dlt(chaos) -> ?DLT_CHAOS;
dlt(ieee802) -> ?DLT_IEEE802;
dlt(arcnet) -> ?DLT_ARCNET;
dlt(slip) -> ?DLT_SLIP;
dlt(ppp) -> ?DLT_PPP;
dlt(fddi) -> ?DLT_FDDI;
dlt(atm_rfc1483) -> ?DLT_ATM_RFC1483;
dlt(raw) -> ?DLT_RAW;
dlt(slip_bsdos) -> ?DLT_SLIP_BSDOS;
dlt(ppp_bsdos) -> ?DLT_PPP_BSDOS;
dlt(pfsync) -> ?DLT_PFSYNC;
dlt(atm_clip) -> ?DLT_ATM_CLIP;
dlt(ppp_serial) -> ?DLT_PPP_SERIAL;
dlt(c_hdlc) -> ?DLT_C_HDLC;
dlt(chdlc) -> ?DLT_CHDLC;
dlt(ieee802_11) -> ?DLT_IEEE802_11;
dlt(loop) -> ?DLT_LOOP;
dlt(linux_sll) -> ?DLT_LINUX_SLL;
dlt(pflog) -> ?DLT_PFLOG;
dlt(ieee802_11_radio) -> ?DLT_IEEE802_11_RADIO;
dlt(apple_ip_over_ieee1394) -> ?DLT_APPLE_IP_OVER_IEEE1394;
dlt(ieee802_22_radio_avs) -> ?DLT_IEEE802_11_RADIO_AVS.


init(Socket, Dev) ->
    % Set the interface for the bpf
    {ok, _} = ctl(Socket, setif, Dev),

    % Allow caller to provide packet header (header complete)
    {ok, _} = ctl(Socket, hdrcmplt, true),

    % Return packets sent from the interface
    {ok, _} = ctl(Socket, seesent, true),

    % Get bpf buf len
    ctl(Socket, blen).
