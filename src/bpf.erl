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

-export([
        open/1,
        data/1,
        promiscuous/1,
        attr/2, attr/3
    ]).
-export([pad/1, align/1]).

-define(SIZEOF_STRUCT_IFREQ, 32).
-define(SIZEOF_INT32_T, 4).
-define(SIZEOF_U_INT, ?SIZEOF_INT32_T).

%% struct bpf_program {
%%         u_int bf_len;
%%         struct bpf_insn *bf_insns;
%% };

%% struct bpf_insn {
%%         u_short     code;
%%         u_char      jt;
%%         u_char      jf;
%%         bpf_u_int32 k;
%% };
-define(SIZEOF_STRUCT_BPF_PROGRAM,
    ?SIZEOF_U_INT + 2 + 1 + 1 + ?SIZEOF_U_INT).

%% struct bpf_stat {
%%         u_int bs_recv;
%%         u_int bs_drop;
%% };
-define(SIZEOF_STRUCT_BPF_STAT,
    ?SIZEOF_U_INT + ?SIZEOF_U_INT)

%% struct bpf_version {
%%         u_short bv_major;
%%         u_short bv_minor;
%% };
-define(SIZEOF_STRUCT_BPF_VERSION, 2 + 2).

%% struct bpf_dltlist {
%%     u_int32_t       bfl_len;
%%     union {
%%         u_int32_t   *bflu_list;
%%         u_int64_t   bflu_pad;
%%     } bfl_u;
%% };
-define(SIZEOF_STRUCT_BPF_DLTLIST,
    ?SIZEOF_U_INT + ?SIZEOF_U_INT + 8).

-define(BPF_ALIGNMENT, ?SIZEOF_INT32_T).

-define(IOC_IN, 16#80000000).
-define(IOC_OUT, 16#40000000).
-define(IOC_VOID, 16#20000000).
-define(IOCPARM_MASK, 16#1fff).

-define(BIOCGBLEN, ior($B, 102, ?SIZEOF_U_INT)).
-define(BIOCSBLEN, iowr($B, 102, ?SIZEOF_U_INT)).
-define(BIOCSETF, iow($B, 103, ?SIZEOF_STRUCT_BPF_PROGRAM)).
-define(BIOCFLUSH, io($B, 104)).
-define(BIOCPROMISC, io($B, 105)).
-define(BIOCGDLT, ior($B,106, ?SIZEOF_U_INT)).
-define(BIOCGETIF, ior($B,107, ?SIZEOF_STRUCT_IFREQ)).
-define(BIOCSETIF, iow($B, 108, ?SIZEOF_STRUCT_IFREQ)).
-define(BIOCSRTIMEOUT, iow($B, 109, sizeof(timeval)).
-define(BIOCGRTIMEOUT, ior($B, 110, sizeof(timeval)).
-define(BIOCGSTATS, ior($B, 111, ?SIZEOF_STRUCT_BPF_STAT)).
-define(BIOCIMMEDIATE, iow($B, 112, ?SIZEOF_U_INT)).
-define(BIOCVERSION, ior($B, 113, SIZEOF_STRUCT_BPF_VERSION)).
-define(BIOCGRSIG, ior($B, 114, ?SIZEOF_U_INT)).
-define(BIOCSRSIG, iow($B, 115, ?SIZEOF_U_INT)).
-define(BIOCGHDRCMPLT, ior($B, 116, ?SIZEOF_U_INT)).
-define(BIOCSHDRCMPLT, iow($B, 117, ?SIZEOF_U_INT)).
-define(BIOCGSEESENT, ior($B, 118, ?SIZEOF_U_INT)).
-define(BIOCSSEESENT, iow($B, 119, ?SIZEOF_U_INT)).
-define(BIOCSDLT, iow($B, 120, ?SIZEOF_U_INT)).
-define(BIOCGDLTLIST, iowr($B, 121, ?SIZEOF_STRUCT_BPF_DLTLIST)).


open(Dev) ->
    {ok, Socket} = procket:open(0, [{dev, "bpf"}]),

    % Set the interface for the bpf
    {ok, _} = attr(Socket, setif, Dev),

    % Allow caller to provide packet header (header complete)
    {ok, _} = attr(Socket, hdrcmplt, true),

    % Return packets sent from the interface
    {ok, _} = attr(Socket, seesent, true),

    % Get bpf buf len
    {ok, Len} = attr(Socket, blen),

    {ok, Socket, Len}.


attr(Socket, blen) ->
    case procket:ioctl(Socket, ?BIOCGBLEN, <<1:32/native>>) of
        {ok, Len} -> {ok, procket:ntohl(Len)};
        Error -> Error
    end.

attr(Socket, setif, Ifname) ->
    % struct ifreq
    Ifreq = list_to_binary([
        Ifname, <<0:((15*8) - (length(Ifname)*8)), 0:8>>,
        <<0:(16*8)>>
    ]),
    procket:ioctl(Socket, ?BIOCSETIF, Ifreq);

attr(Socket, immediate, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCIMMEDIATE, bool(Bool));

attr(Socket, hdrcmplt, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCSHDRCMPLT, bool(Bool));

attr(Socket, seesent, Bool) when Bool == true; Bool == false ->
    procket:ioctl(Socket, ?BIOCSSEESENT, bool(Bool)).


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

sizeof(timeval) ->
    erlang:system_info({wordsize, external}) + ?SIZEOF_U_INT.
