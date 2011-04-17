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
        promiscuous/1
    ]).
-export([pad/1, align/1]).

-define(SIZEOF_STRUCT_IFREQ, 32).
-define(SIZEOF_INT32_T, 4).
-define(SIZEOF_U_INT, ?SIZEOF_INT32_T).

-define(BPF_ALIGNMENT, ?SIZEOF_INT32_T).

-define(IOC_IN, 16#80000000).
-define(IOC_OUT, 16#40000000).
-define(IOC_VOID, 16#20000000).
-define(IOCPARM_MASK, 16#1fff).

-define(BIOCGBLEN, ior($B,102, ?SIZEOF_U_INT)).
-define(BIOCPROMISC, io($B,105)).
-define(BIOCSETIF, iow($B, 108, ?SIZEOF_STRUCT_IFREQ)).
-define(BIOCIMMEDIATE, iow($B, 112, ?SIZEOF_U_INT)).


open(Dev) ->
    {ok, Socket} = procket:open(0, [{bpf, true}]),

    % struct ifreq
    Ifreq = list_to_binary([
        Dev, <<0:((15*8) - (length(Dev)*8)), 0:8>>,
        <<0:(16*8)>>
    ]),

    % Set the interface for the bpf
    {ok, _} = procket:ioctl(Socket, ?BIOCSETIF, Ifreq),

    % Set immediate mode (reads/writes return immediately)
    {ok, _} = procket:ioctl(Socket, ?BIOCIMMEDIATE, <<1:32/native>>),

    % Get bpf buf len
    {ok, Len} = procket:ioctl(Socket, ?BIOCGBLEN, <<1:32/native>>),

    {ok, Socket, procket:ntohl(Len)}.


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
