%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
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

%% Support for msghdr and cmsghdr structures
-module(procket_msg).
-include("procket_msg.hrl").

-export([
        iovec/1,
        cmsghdr/1,
        msghdr/1
        ]).

%%
%% iovec
%%

%    struct iovec
%    {
%        void *iov_base; /* BSD uses caddr_t (1003.1g requires void *) */
%        __kernel_size_t iov_len; /* Must be size_t (1003.1g) */
%    };
iovec([]) ->
    {ok, <<>>, []};
iovec(Vec) when is_list(Vec) ->
    Size = erlang:system_info({wordsize, external}),
    Iov = lists:flatmap(fun(Buf) ->
            Len = byte_size(Buf),
            [{ptr, Buf}, <<Len:Size/native-unsigned-integer-unit:8>>]
        end, Vec),
    procket:alloc(Iov).


%%
%% cmsghdr
%%

% Linux uses a size_t for the control message header length. Everyone
% else uses a socklen_t, which is really an int. See "The socklen_t type"
% in the NOTES section of the Linux accept(2) man page for details. RFC
% 3542 has this explanation:
%
% Note: Before Posix [...] the [...] socklen_t members were typically
% integers.  Earlier drafts of Posix had the [...] socklen_t members
% as size_t, but it then changed these to socklen_t to simplify binary
% portability for 64-bit implementations and to align Posix with X/Open's
% Networking Services, Issue 5.

%% Linux
%struct cmsghdr
%  {
%    size_t cmsg_len;    /* Length of data in cmsg_data plus length
%                           of cmsghdr structure.
%                           !! The type should be socklen_t but the
%                           definition of the kernel is incompatible
%                           with this.  */
%    int cmsg_level;     /* Originating protocol.  */
%    int cmsg_type;      /* Protocol specific type.  */
%};
%
%% BSD
%struct cmsghdr {
%    socklen_t   cmsg_len;   /* data byte count, including hdr */
%    int     cmsg_level; /* originating protocol */
%    int     cmsg_type;  /* protocol-specific type */
%};
cmsghdr(#cmsghdr{level = Level, type = Type, data = Data}) ->
    Size = case os:type() of
        {unix, linux} ->
            erlang:system_info({wordsize, external});
        {unix, _} ->
            4
    end,
    HdrLen = Size + 4 + 4,
    Pad0 = procket:wordalign(HdrLen),
    Pad1 = procket:wordalign(HdrLen + Pad0 + byte_size(Data)),
    Len = HdrLen + Pad0 + byte_size(Data),
    <<Len:Size/native-unsigned-integer-unit:8,
      Level:4/native-unsigned-integer-unit:8,
      Type:4/native-unsigned-integer-unit:8,
      0:(Pad0 * 8),
      Data/binary,
      0:(Pad1 * 8)>>;
cmsghdr(Buf) when is_binary(Buf) ->
    Size = case os:type() of
        {unix, linux} ->
            erlang:system_info({wordsize, external});
        {unix, _} ->
            4
    end,
    HdrLen = Size + 4 + 4,
    Pad0 = procket:wordalign(HdrLen) * 8,
    <<Len:Size/native-unsigned-integer-unit:8,
      Level:4/native-unsigned-integer-unit:8,
      Type:4/native-unsigned-integer-unit:8,
      _:Pad0,
      Rest/binary>> = Buf,
    DataLen = Len - HdrLen,
    Pad1 = procket:wordalign(HdrLen + procket:wordalign(HdrLen) + DataLen) * 8,
    <<Data:DataLen/bytes, _:Pad1, Next/binary>> = Rest,
    {#cmsghdr{
        len = Len,
        level = Level,
        type = Type,
        data = Data
        }, Next}.


%%
%% msghdr
%%

% struct msghdr {
%     void      *msg_name;        /* ptr to socket address
%                                    structure */
%     socklen_t  msg_namelen;     /* size of socket address
%                                    structure */
%     struct iovec  *msg_iov;     /* scatter/gather array */
%     int        msg_iovlen;      /* # elements in msg_iov */
%     void      *msg_control;     /* ancillary data */
%     socklen_t  msg_controllen;  /* ancillary data buffer length */
%     int        msg_flags;       /* flags on received message */
% };
msghdr(#msghdr{name = Name, iov = Iov, control = Control, flags = Flags}) ->
    Size = erlang:system_info({wordsize, external}),
    case iovec(Iov) of
        {ok, Vec, Res} ->
            Alloc = [ Field || {Field, Value} <-
                [{msg_name, Name}, {msg_iov, Vec}, {msg_control, Control}, {iov, Res}],
                Value /= <<>>, Value /= [] ],
            Pad0 = procket:wordalign(Size + 4),
            Pad1 = procket:wordalign(Size + 4 + Pad0 + Size + Size + Size + Size + 4),
            Msg = [
                ptr(Name, Size),
                <<(byte_size(Name)):4/native-unsigned-integer-unit:8>>,
                <<0:(Pad0 * 8)>>,
                ptr(Vec, Size),
                <<(length(Iov)):Size/native-unsigned-integer-unit:8>>,
                ptr(Control, Size),
                <<(byte_size(Control)):Size/native-unsigned-integer-unit:8>>,
                <<Flags:4/native-unsigned-integer-unit:8>>,
                <<0:(Pad1 * 8)>>
            ],
            msghdr_1(Alloc, Msg, Res);
        Error ->
            Error
    end;
msghdr(Buf) when is_binary(Buf) ->
    Size = erlang:system_info({wordsize, external}),
    Pad0 = procket:wordalign(Size + 4) * 8,
    Pad1 = procket:wordalign(Size + 4 + procket:wordalign(Size + 4)
        + Size + Size + Size + Size + 4) * 8,
    <<Name:Size/bytes,
      NameLen:4/native-unsigned-integer-unit:8,
      _:Pad0,
      Iov:Size/bytes,
      IovLen:Size/native-unsigned-integer-unit:8,
      Control:Size/bytes,
      ControlLen:Size/native-unsigned-integer-unit:8,
      Flags:4/native-unsigned-integer-unit:8,
      _:Pad1>> = Buf,

    #msghdr{
        name = Name,
        namelen = NameLen,
        iov = Iov,
        iovlen = IovLen,
        control = Control,
        controllen = ControlLen,
        flags = Flags
    }.

msghdr_1(Alloc, Msg, IOVRes) ->
    case procket:alloc(Msg) of
        {ok, Struct, Res} ->
            {ok, Struct, lists:zip(Alloc, Res ++ [IOVRes])};
        Error ->
            Error
    end.

ptr(<<>>, Size) -> <<0:(Size * 8)>>;
ptr(N, _Size) -> {ptr, N}.
