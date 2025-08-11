%%% @copyright 2010-2025 Michael Santos <michael.santos@gmail.com>
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

%% @doc procket is an Erlang library for socket creation and manipulation.
%%
%% procket can use a setuid helper so actions like binding low ports and
%% requesting some sockets types can be done while Erlang is running as an
%% unprivileged user.
%%
%% ## FEATURES
%%
%% Other features include:
%%
%% * low level socket manipulation using socket/3, ioctl/3, setsockopt/4, ...
%%
%% * support any protocols supported by the socket interface: ICMP, Unix
%%   sockets, ...
%%
%% * support for the BSD raw socket interface
%%
%% * generate and snoop packets using PF_PACKET sockets on Linux
%%
%% * generate and snoop packets using the BPF interface on BSDs like Mac OS X
%%
%% * support for creating and reading/writing from character devices like
%%   TUN/TAP interfaces
%%
%% ## SETUID vs SUDO vs Capabilities
%%
%% The procket helper executable needs root privileges. Either allow your
%% user to run procket using sudo or copy procket to somewhere owned by
%% root and make it setuid.
%%
%% * for sudo
%%
%% ```
%% sudo visudo
%% youruser ALL=NOPASSWD: /path/to/procket/priv/procket
%%
%% # if sudoers has enabled "Default requiretty", you will need to set
%% # one of these options too:
%%
%% Defaults!/path/to/procket/priv/procket !requiretty
%% Defaults:youruser !requiretty
%% '''
%%
%% * to make it setuid
%%
%% ```
%% # Default location
%% sudo chown root priv/procket
%% sudo chmod 4750 priv/procket
%% '''
%%
%% The procket setuid helper can also be placed in a system directory:
%%
%% ```
%% # System directory
%% sudo cp priv/procket /usr/local/bin
%% sudo chown root:yourgroup /usr/local/bin/procket
%% sudo chmod 4750 /usr/local/bin/procket
%% '''
%%
%% Then pass the progname argument to open/2:
%%
%% ```
%% {ok, FD} = procket:open(53, [{progname, "/usr/local/bin/procket"},
%%         {protocol, udp},{type, dgram},{family, inet}]).
%% '''
%%
%% * use Linux capabilities: beam or the user running beam can be
%%   given whatever socket privileges are needed. For example, using file
%%   capabilities:
%%
%% ```
%% setcap cap_net_raw=ep /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp
%% '''
%%
%% To see the capabilities:
%%
%% ```
%% getcap /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp
%% '''
%%
%% To remove the capabilities:
%%
%% ```
%% setcap -r /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp
%% '''
-module(procket).
-include("procket.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    open/1, open/2,
    dev/1, dev/2,
    socket/3,
    listen/1, listen/2,
    connect/2,
    accept/1, accept/2,
    close/1,
    recv/2,
    recvfrom/2, recvfrom/4,
    sendto/2, sendto/3, sendto/4,
    read/2,
    write/2,
    writev/2,
    bind/2,
    ioctl/3,
    setsockopt/4,
    getsockopt/4,
    getsockname/2,

    recvmsg/4, recvmsg/5,
    sendmsg/4, sendmsg/5,

    setns/1, setns/2,

    family/1,

    alloc/1,
    buf/1,
    memcpy/2,
    wordalign/1, wordalign/2,

    socket_level/0, socket_level/1,
    socket_optname/0, socket_optname/1,
    socket_protocol/0, socket_protocol/1,

    errno_id/1,

    set_sock_nonblock/1
]).
-export([
    unix_path_max/0,
    sockaddr_common/2,
    ntohl/1,
    ntohs/1
]).
% for debugging
-export([
    getopts/1,
    progname/0
]).

-type uint16_t() :: 0..16#ffff.
-type uint64_t() :: 0..16#ffffffffffffffff.
-type uint32_t() :: 0..16#ffffffff.
-type int32_t() :: -16#7fffffff..16#7fffffff.

-type size_t() :: uint64_t().

-type fd() :: int32_t().
-type posix() ::
    e2big
    | eacces
    | eaddrinuse
    | eaddrnotavail
    | eadv
    | eafnosupport
    | eagain
    | ealign
    | ealready
    | ebade
    | ebadf
    | ebadfd
    | ebadmsg
    | ebadr
    | ebadrpc
    | ebadrqc
    | ebadslt
    | ebfont
    | ebusy
    | ecapmode
    | echild
    | echrng
    | ecomm
    | econnaborted
    | econnrefused
    | econnreset
    | edeadlk
    | edeadlock
    | edestaddrreq
    | edirty
    | edom
    | edotdot
    | edquot
    | eduppkg
    | eexist
    | efault
    | efbig
    | ehostdown
    | ehostunreach
    | eidrm
    | einit
    | einprogress
    | eintr
    | einval
    | eio
    | eisconn
    | eisdir
    | eisnam
    | el2hlt
    | el2nsync
    | el3hlt
    | el3rst
    | elbin
    | elibacc
    | elibbad
    | elibexec
    | elibmax
    | elibscn
    | elnrng
    | eloop
    | emfile
    | emlink
    | emsgsize
    | emultihop
    | enametoolong
    | enavail
    | enet
    | enetdown
    | enetreset
    | enetunreach
    | enfile
    | enoano
    | enobufs
    | enocsi
    | enodata
    | enodev
    | enoent
    | enoexec
    | enolck
    | enolink
    | enomem
    | enomsg
    | enonet
    | enopkg
    | enoprotoopt
    | enospc
    | enosr
    | enostr
    | enosym
    | enosys
    | enotblk
    | enotcapable
    | enotconn
    | enotdir
    | enotempty
    | enotnam
    | enotrecoverable
    | enotsock
    | enotsup
    | enotty
    | enotuniq
    | enxio
    | eopnotsupp
    | eoverflow
    | eownerdead
    | eperm
    | epfnosupport
    | epipe
    | eproclim
    | eprocunavail
    | eprogmismatch
    | eprogunavail
    | eproto
    | eprotonosupport
    | eprototype
    | erange
    | erefused
    | eremchg
    | eremdev
    | eremote
    | eremoteio
    | eremoterelease
    | erofs
    | erpcmismatch
    | erremote
    | eshutdown
    | esocktnosupport
    | espipe
    | esrch
    | esrmnt
    | estale
    | esuccess
    | etime
    | etimedout
    | etoomanyrefs
    | etxtbsy
    | euclean
    | eunatch
    | eusers
    | eversion
    | ewouldblock
    | exdev
    | exfull.

-type protocol() :: ip | icmp | tcp | udp | 'ipv6-icmp' | raw.
-type type() :: stream | dgram | raw | seqpacket.
-type family() :: unspec | inet | inet6 | netlink | packet | local | unix | file.

-type open_opt() ::
    {protocol, protocol() | integer()}
    | {type, type() | integer()}
    | {family, family() | integer()}
    | {ip, inet:ip_address()}
    | {dev, string()}
    | {exec, [string()]}
    | {progname, string()}
    | {interface, string()}
    | {pipe, string()}
    | {namespace, string()}.

-export_type([
    uint16_t/0,
    uint64_t/0,
    int32_t/0,
    size_t/0,
    fd/0,
    posix/0,

    protocol/0,
    type/0,
    family/0,

    open_opt/0
]).

-on_load(on_load/0).

on_load() ->
    erlang:load_nif(progname(), []).

%%--------------------------------------------------------------------
%%% NIF Stubs
%%--------------------------------------------------------------------

% @doc close(2): close a file descriptor
%
% == Examples ==
%
% ```
% 1> procket:open(8080).
% {ok,22}
%
% 2> procket:close(22)
% ok
% '''
-spec close(Socket :: integer()) -> ok | {error, posix()}.
close(_) ->
    erlang:nif_error(not_implemented).

fdrecv(_) ->
    erlang:nif_error(not_implemented).

% @doc accept(2): accept a connection on a socket
%
% accept/1 returns the file descriptor associated with the new connection.
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
% 4> procket:listen(S).
% ok
% 5> procket:accept(S).
% {error,eagain}
%
% % Connect: nc localhost 10022
% 6> procket:accept(S).
% {ok,21}
% '''
-spec accept(Socket :: integer()) -> {ok, fd()} | {error, posix()}.
accept(Socket) ->
    case accept(Socket, 0) of
        {ok, FD, _} -> {ok, FD};
        Error -> Error
    end.

% @doc accept(2): accept a connection on a socket
%
% accept/2 will allocate a struct sockaddr of size Salen bytes that will
% hold the peer address. If the size is too small, the returned binary
% will be zero padded to indicate the size required.
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
% 4> procket:listen(S).
% ok
% 5> procket:accept(S, 16).
% {error,eagain}
%
% % Connect: nc localhost 10022
% 6> procket:accept(S, 16).
% {ok,22,<<2,0,207,110,127,0,0,1,0,0,0,0,0,0,0,0>>}
% '''
-spec accept(Socket :: integer(), Salen :: integer()) -> {ok, fd(), binary()} | {error, posix()}.
accept(_, _) ->
    erlang:nif_error(not_implemented).

% @doc bind(2): bind a name to a socket
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
% '''
-spec bind(Socket :: integer(), Sockaddr :: binary()) -> ok | {error, posix()}.
bind(_, _) ->
    erlang:nif_error(not_implemented).

% @doc connect(2): initiate a connection on a socket
%
% Sockaddr is a struct sockaddr whose layout is dependent on
% platform. If Sockaddr is an empty binary, connect(2) will be
% called with NULL as the second option.
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                22:16,          % Port
%                127,0,0,1,      % IPv4 loopback
%                0:64
%                >>.
% <<2,0,0,22,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:connect(S, Sockaddr).
% {error,einprogress}
% 4> procket:connect(S, Sockaddr).
% {error,econnrefused}
% '''
-spec connect(Socket :: integer(), Sockaddr :: binary()) -> ok | {error, posix()}.
connect(_, _) ->
    erlang:nif_error(not_implemented).

% @doc listen(2): listen for connections on a socket
%
% listen/1 sets the backlog to 50.
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> procket:listen(S).
% ok
% '''
-spec listen(Socket :: integer()) -> ok | {error, posix()}.
listen(Socket) when is_integer(Socket) ->
    listen(Socket, ?BACKLOG).

% @doc listen(2): listen for connections on a socket
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> procket:listen(S, 16#ffff).
% ok
% '''
-spec listen(Socket :: integer(), Backlog :: integer()) -> ok | {error, posix()}.
listen(_, _) ->
    erlang:nif_error(not_implemented).

% @doc recv(2): receive a message from a socket
-spec recv(Socket :: integer(), Size :: size_t()) -> {ok, binary()} | {error, posix()}.
recv(Socket, Size) ->
    recvfrom(Socket, Size).

% @doc recvfrom(2): receive a message from a socket
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
% 4> procket:listen(S).
% ok
%
% % echo "test test 123" | nc localhost 10022
% 5> {ok, FD, _} = procket:accept(S, 16).
% {ok,21,<<2,0,207,110,127,0,0,1,0,0,0,0,0,0,0,0>>}
%
% 6> procket:recvfrom(FD, 1024).
% {ok,<<"test test 123\n">>}
% '''
-spec recvfrom(Socket :: integer(), Size :: size_t()) -> {ok, binary()} | {error, posix()}.
recvfrom(Socket, Size) ->
    case recvfrom(Socket, Size, 0, 0) of
        {ok, Buf, _} -> {ok, Buf};
        Error -> Error
    end.

% @doc recvfrom(2): receive a message from a socket
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, dgram, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
%
% % echo "test test 123" | nc -u localhost 10022
% 4> procket:recvfrom(S, 1024, 0, byte_size(Sockaddr)).
% {ok,<<"test test 123\n">>,
%     <<2,0,173,123,127,0,0,1,0,0,0,0,0,0,0,0>>}
% '''
-spec recvfrom(Socket :: integer(), Size :: size_t(), Flags :: integer(), Salen :: size_t()) ->
    {ok, binary(), binary()} | {error, posix()}.
recvfrom(_, _, _, _) ->
    erlang:nif_error(not_implemented).

% @doc read(2): read bytes from a file descriptor
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
% 4> procket:listen(S).
% ok
%
% % echo "test test 123" | nc localhost 10022
% 5> {ok, FD, _} = procket:accept(S, 16).
% {ok,21,<<2,0,207,110,127,0,0,1,0,0,0,0,0,0,0,0>>}
%
% 6> procket:read(FD, 1024).
% {ok,<<"test test 123\n">>}
% '''
-spec read(FD :: integer(), Length :: size_t()) -> {ok, binary()} | {error, posix()}.
read(_, _) ->
    erlang:nif_error(not_implemented).

% @doc socket(2): returns a file descriptor for a communication endpoint
%
% == Examples ==
%
% ```
% 1> procket:socket(inet, stream, 0).
% {ok,20}
% '''
-spec socket(
    Family :: family() | integer(), Type :: type() | integer(), Protocol :: protocol() | integer()
) -> {ok, fd()} | {error, posix()}.
socket(Family, Type, Protocol) ->
    socket_nif(
        maybe_atom(family, Family),
        maybe_atom(type, Type),
        maybe_atom(protocol, Protocol)
    ).
socket_nif(_, _, _) ->
    erlang:nif_error(not_implemented).

% @doc setns(2): reassociate thread with a namespace, joining any namespace.
-spec setns(ProcPath :: iolist()) -> ok | {error, posix()}.
setns(ProcPath) ->
    setns(ProcPath, 0).
-spec setns(ProcPath :: iolist(), NSType :: integer()) -> ok | {error, posix()}.
setns(_, _) ->
    erlang:nif_error(not_implemented).

% @doc ioctl(2): control device
%
% Be careful with this function.
%
% Request is an integer with the direction of the request encoded
% into it (IN, OUT, IN/OUT). Result is a binary holding the result.
% If the ioctl is IN only, the Result will be the same as Arg.
%
% Arg is a structure dependent on the request.
%
% See procket_ioctl.erl for some helper functions for dealing
% with ioctl.
%
% Caveats:
%
% * Request is an integer on Linux and an unsigned long on OS X
%
% * some ioctl requests require a structure with a pointer to
%   memory. Use alloc/1 to create these structures and buf/1 to
%   retrieve the data from them.
%
% * some ioctl requests use an integer instead of a pointer to
%   a structure. This means that it's possible to pass in an
%   arbitrary pointer (an integer) as an argument to an ioctl
%   expecting a structure. Don't do this.
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
%
% % SIOCGIFINDEX = 16#8933
% 2> {ok, <<Ifname:16/bytes, Ifr:32/native, _/binary>>} = procket:ioctl(S, 16#8933, <<"eth0", 0:224>>).
%       {ok,<<101,116,104,48,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,
%             0,0,0,0,0,...>>}
% 4> Ifname.
% <<101,116,104,48,0,0,0,0,0,0,0,0,0,0,0,0>>
% 5> Ifr.
% 7
% '''
-spec ioctl(FD :: integer(), Request :: uint64_t(), Arg :: binary() | integer()) ->
    {ok, binary()} | {error, posix()}.
ioctl(_, _, _) ->
    erlang:nif_error(not_implemented).

% @doc Return the contents of memory allocated using alloc/1.
%
% @see alloc/1
-spec buf(Resource :: reference()) -> {ok, binary()} | {error, enomem}.
buf(_) ->
    erlang:nif_error(not_implemented).

% @doc Write data to memory allocated using alloc/1.
%
% == Examples ==
%
% ```
% 1> {ok, Buf, [Ref]} = procket:alloc([{ptr, 1024}]).
% {ok,<<112,125,4,72,127,0,0,0>>,
%     [#Ref<0.4091636758.1194983426.105614>]}
% 2> procket:memcpy(Ref, <<1,2,3,4,5,6,7,8,9>>).
% ok
% 3> procket:buf(Ref).
% {ok,<<1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%      0,0,...>>}
% '''
%
% @see alloc/1
-spec memcpy(Dest :: reference(), Src :: binary()) -> ok.
memcpy(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Allocate structure for ioctl/3.
%
% Create a structure containing pointers to memory that can be
% passed as the third argument to ioctl/3.
%
% The size of the allocated memory can be indicated by either
% using an integer or passing in a binary of the appropriate size.
% If an integer is used, the contents are zero'ed. If a binary is
% used, the memory is initialized to the contents of the binary.
%
% Resource is a list of NIF resources (one for each piece of
% allocated memory) requested in the struct. The memory will
% automatically be freed by the resource.
%
% It is up to the caller to ensure the structure has the proper
% endianness and alignment for the platform.
%
% == Examples ==
%
% For example, a struct bpf_program is used to set a filter on a
% bpf character device:
%
% ```
% struct bpf_program {
%     u_int bf_len;
%     struct bpf_insn *bf_insns;
% };
%
% struct bpf_insn {
%     u_short     code;
%     u_char      jt;
%     u_char      jf;
%     bpf_u_int32 k;
% };
% '''
%
% To allocate a binary in Erlang:
%
% ```
% Insn = [
%     ?BPF_STMT(?BPF_LD+?BPF_H+?BPF_ABS, 12),                     % offset = Ethernet Type
%     ?BPF_JUMP(?BPF_JMP+?BPF_JEQ+?BPF_K, ?ETHERTYPE_IP, 0, 1),   % type = IP
%
%     ?BPF_STMT(?BPF_RET+?BPF_K, 16#FFFFFFFF),                    % return: entire packet
%     ?BPF_STMT(?BPF_RET+?BPF_K, 0)                               % return: drop packet
% ],
% {ok, Code, [Res]} = procket:alloc([
%     <<(length(Insn)):4/native-unsigned-integer-unit:8>>,
%     {ptr, list_to_binary(Insn)}
% ]).
% '''
%
% To use the ioctl and return the contents of the memory:
%
% ```
% case procket:ioctl(Socket, ?BIOCSETF, Code) of
%     {ok, _} ->
%         procket:buf(Res);
%     Error ->
%         Error
% end.
% '''
%
% @see ioctl/3
-spec alloc([binary() | {ptr, size_t()} | {ptr, binary()}]) ->
    {ok, binary(), [reference()]} | {error, posix()}.
alloc(Struct) ->
    case alloc_nif(Struct) of
        {ok, Bin, Res} ->
            {ok, Bin, lists:reverse(Res)};
        N ->
            N
    end.
alloc_nif(_) ->
    erlang:nif_error(not_implemented).

% @doc sendto(2): send a message on a socket
-spec sendto(Socket :: integer(), Buf :: binary()) -> ok | {error, posix()}.
sendto(Socket, Buf) ->
    sendto(Socket, Buf, 0, <<>>).

% @doc sendto(2): send a message on a socket
-spec sendto(Socket :: integer(), Buf :: binary(), Flags :: integer()) -> ok | {error, posix()}.
sendto(Socket, Buf, Flags) ->
    sendto(Socket, Buf, Flags, <<>>).

% @doc sendto(2): send a message on a socket
-spec sendto(Socket :: integer(), Buf :: binary(), Flags :: integer(), Sockaddr :: binary()) ->
    ok | {ok, size_t()} | {error, posix()}.
sendto(Socket, Buf, Flags, Sockaddr) ->
    Size = byte_size(Buf),
    case sendto_nif(Socket, Buf, Flags, Sockaddr) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

sendto_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).

-spec write(FD :: integer(), Buf :: binary() | [binary()]) ->
    ok | {ok, size_t()} | {error, posix()}.
write(FD, Buf) when is_binary(Buf) ->
    Size = byte_size(Buf),
    case write_nif(FD, Buf) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end;
write(FD, Buf) when is_list(Buf) ->
    writev(FD, Buf).

write_nif(_, _) ->
    erlang:nif_error(not_implemented).

-spec writev(FD :: integer(), Bufs :: [binary()]) -> ok | {ok, size_t()} | {error, posix()}.
writev(FD, Buf) ->
    Size = iolist_size(Buf),
    case writev_nif(FD, Buf) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

writev_nif(_, _) ->
    erlang:nif_error(not_implemented).

% @doc recvmsg(2): receive a message from a socket
-spec recvmsg(Socket :: integer(), Size :: size_t(), Flags :: integer(), CtrlDataSize :: size_t()) ->
    {ok, binary(), integer(), [{integer(), integer(), binary()}]} | {error, posix()}.
recvmsg(Socket, Size, Flags, CtrlDataSize) ->
    case recvmsg(Socket, Size, Flags, CtrlDataSize, 0) of
        {ok, Buf, Flags, CtrlData, <<>>} ->
            {ok, Buf, Flags, CtrlData};
        {error, _} = Error ->
            Error
    end.

% @doc recvmsg(2): receive a message from a socket
-spec recvmsg(
    Socket :: integer(),
    Size :: size_t(),
    Flags :: integer(),
    CtrlDataSize :: size_t(),
    SockaddrSize :: size_t()
) -> {ok, binary(), integer(), [{integer(), integer(), binary()}], binary()} | {error, posix()}.
recvmsg(Socket, Size, Flags, CtrlDataSize, SockaddrSize) ->
    case recvmsg_nif(Socket, Size, Flags, CtrlDataSize, SockaddrSize) of
        {ok, Buf, Flags, CtrlData, Sockaddr} ->
            {ok, Buf, Flags, lists:reverse(CtrlData), Sockaddr};
        N ->
            N
    end.

recvmsg_nif(_, _, _, _, _) ->
    erlang:nif_error(not_implemented).

% @doc sendmsg(2): send a message on a socket
-spec sendmsg(
    Socket :: integer(),
    Buf :: binary(),
    Flags :: integer(),
    CtrlData :: [{integer(), integer(), binary()}]
) -> ok | {error, posix()}.
sendmsg(Socket, Buf, Flags, CtrlData) ->
    sendmsg(Socket, Buf, Flags, CtrlData, <<>>).

% @doc sendmsg(2): send a message on a socket
-spec sendmsg(
    Socket :: integer(),
    Buf :: binary(),
    Flags :: integer(),
    CtrlData :: [{integer(), integer(), binary()}],
    Sockaddr :: binary()
) -> ok | {ok, size_t()} | {error, posix()}.
sendmsg(Socket, Buf, Flags, CtrlData, Sockaddr) ->
    Size = byte_size(Buf),
    case sendmsg_nif(Socket, Buf, Flags, CtrlData, Sockaddr) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

sendmsg_nif(_, _, _, _, _) ->
    erlang:nif_error(not_implemented).

% @doc setsockopt(2): set options on sockets
%
% Level and Optname can either be an integer or an atom with the
% same name as the definitions in the system header files, e.g.,
% 'IPPROTO_IPIP', 'SO_REUSEPORT'. Note these are uppercase atoms
% and so must be quoted.
%
% If an atom is used as an argument and is not supported by the OS,
% setsockopt/4 will return {error,unsupported}.
%
% == Examples ==
%
% ```
% 1> {ok, FD} = procket:open(0, [{protocol, 'ipv6-icmp'}, {type, raw}, {family, inet6}]).
% {ok,28}
%
% % IPPROTO_V6 = 41, Linux: IPV6_UNICAST_HOPS = 16 set to 255
% 2> procket:setsockopt(FD, 41, 16, <<255:4/native-unsigned-integer-unit:8>>).
% ok
%
% % : IPPROTO_V6 = 41, Linux: IPV6_MULTICAST_HOPS = 16 set to 255
% 3> procket:setsockopt(FD, 41, 18, <<255:4/native-unsigned-integer-unit:8>>).
% ok
% '''
-spec setsockopt(
    Socket :: integer(),
    Level :: integer() | atom(),
    Optname :: integer() | atom(),
    Optval :: binary()
) -> ok | {error, posix() | unsupported}.
setsockopt(Socket, Level, Optname, Optval) when is_atom(Level) ->
    case
        socket_constant_foreach(
            Level,
            [fun socket_level/1, fun socket_protocol/1]
        )
    of
        undefined ->
            {error, unsupported};
        N ->
            setsockopt(Socket, N, Optname, Optval)
    end;
setsockopt(Socket, Level, Optname, Optval) when is_atom(Optname) ->
    case socket_optname(Optname) of
        undefined ->
            {error, unsupported};
        N ->
            setsockopt(Socket, Level, N, Optval)
    end;
setsockopt(Socket, Level, Optname, Optval) ->
    setsockopt_nif(Socket, Level, Optname, Optval).

% @doc getsockopt(2): get options on sockets
%
% Similar to inet:getopts/2 but can be used with file descriptors.
%
% Retrieve a socket option for a file descriptor. Use an empty binary
% to indicate no option value is supplied or will be returned.
%
% == Examples ==
%
% ```
% 1> {ok, FD} = procket:open(0, [{protocol, 'ipv6-icmp'}, {type, raw}, {family, inet6}]).
% {ok,28}
%
% % IPPROTO_V6 = 41, Linux: IPV6_UNICAST_HOPS = 16 set to 255
% 2> procket:setsockopt(FD, 41, 16, <<255:4/native-unsigned-integer-unit:8>>).
% ok
%
% 3> procket:getsockopt(FD, 41, 16, <<0:32>>).
% {ok,<<255,0,0,0>>}
% '''
%
% @see setsockopt/4
-spec getsockopt(
    Socket :: integer(),
    Level :: integer() | atom(),
    Optname :: integer() | atom(),
    Optval :: binary()
) -> {ok, binary()} | {error, posix() | unsupported}.
getsockopt(Socket, Level, Optname, Optval) when is_atom(Level) ->
    case
        socket_constant_foreach(
            Level,
            [fun socket_level/1, fun socket_protocol/1]
        )
    of
        undefined ->
            {error, unsupported};
        N ->
            getsockopt(Socket, N, Optname, Optval)
    end;
getsockopt(Socket, Level, Optname, Optval) when is_atom(Optname) ->
    case socket_optname(Optname) of
        undefined ->
            {error, unsupported};
        N ->
            getsockopt(Socket, Level, N, Optval)
    end;
getsockopt(Socket, Level, Optname, Optval) ->
    getsockopt_nif(Socket, Level, Optname, Optval).

setsockopt_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).
getsockopt_nif(_, _, _, _) ->
    erlang:nif_error(not_implemented).

% @doc getsockname(2): get socket name
%
% If the input binary is too small to hold the socket address structure,
% the returned binary is zero padded to indicate the size required.
%
% == Examples ==
%
% ```
% 1> {ok, S} = procket:socket(inet, stream, 0).
% {ok,20}
% 2> Sockaddr = <<(procket:sockaddr_common(procket:family(inet), 16))/binary,
%                   10022:16,       % Port
%                   127,0,0,1,      % IPv4 loopback
%                   0:64
%               >>.
% <<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>
% 3> procket:bind(S, Sockaddr).
% ok
% 4> procket:getsockname(S, <<0:(byte_size(Sockaddr)*8)>>).
% {ok,<<2,0,39,38,127,0,0,1,0,0,0,0,0,0,0,0>>}
% '''
-spec getsockname(Socket :: integer(), Sockaddr :: binary()) -> {ok, binary()} | {error, posix()}.
getsockname(_, _) ->
    erlang:nif_error(not_implemented).

socket_level() ->
    erlang:nif_error(not_implemented).
socket_level(_) ->
    erlang:nif_error(not_implemented).

socket_optname() ->
    erlang:nif_error(not_implemented).
socket_optname(_) ->
    erlang:nif_error(not_implemented).

socket_protocol() ->
    erlang:nif_error(not_implemented).
socket_protocol(_) ->
    erlang:nif_error(not_implemented).

% @doc Return atom describing error number.
%
% == Examples ==
%
% ```
% 1> procket:errno_id(1).
% eperm
% '''
-spec errno_id(int32_t()) -> posix().
errno_id(_) ->
    erlang:nif_error(not_implemented).

% @doc Set O_NONBLOCK status flag on socket.
%
% Note: file descriptors opened by procket are opened in non-blocking mode.
-spec set_sock_nonblock(Socket :: int32_t()) -> ok | posix().
set_sock_nonblock(_) ->
    erlang:nif_error(not_implemented).

socket_constant_foreach(_Constant, []) ->
    undefined;
socket_constant_foreach(Constant, [Fun | Funs]) ->
    case Fun(Constant) of
        undefined ->
            socket_constant_foreach(Constant, Funs);
        N when is_integer(N) ->
            N
    end.

%%--------------------------------------------------------------------
%%% Setuid helper
%%--------------------------------------------------------------------

% @doc Open a character device such as bpf, tun or tap devices.
%
% Wrapper around open/2.
%
% == Examples ==
%
% ```
% 1> procket:dev("net/tun").
% {ok,22}
% '''
%
% @see open/2
-spec dev(Dev :: string()) -> {ok, fd()} | {error, posix()}.
dev(Dev) when is_list(Dev) ->
    open(0, [{dev, Dev}]).

% @doc Open a character device such as bpf, tun or tap devices with options.
%
% == Examples ==
%
% ```
% # ip netns add foo
% '''
%
% ```
% 1> procket:dev("net/tun", [{namespace, "/var/run/netns/foo"}]).
% {ok,22}
% '''
-spec dev(Dev :: string(), [open_opt()]) -> {ok, fd()} | {error, posix()}.
dev(Dev, Opts) when is_list(Dev), is_list(Opts) ->
    open(0, [{dev, Dev} | Opts]).

% @doc Open a socket or device using the procket setuid helper.
%
% == Examples ==
%
% ```
% 1> procket:open(8080).
% {ok,22}
% '''
-spec open(Port :: uint16_t()) -> {ok, fd()} | {error, posix()}.
open(Port) ->
    open(Port, []).

% @doc Open a socket or device using the procket setuid helper.
%
% The file descriptor is passed back over a Unix socket.
%
% The default behaviour of open/1,2 is to attempt to open the
% socket twice: first by running the procket setuid helper and, if this
% fails because the process does not have the appropriate permissions,
% running the setuid helper again using "sudo". The default behaviour
% can be changed by using the 'exec' option:
%
% ```
% procket:open(Port, [{exec, ["", "sudo"]}]).
% '''
%
% Linux only: the {namespace, string()} option causes the procket
% setuid helper to open the socket in a pre-configured namespace. By
% default, all namespaces are joined by the helper.
%
% == Examples ==
%
% ```
% 1> procket:open(9019, [{exec, [""]}]).
% {ok,24}
% '''
-spec open(Port :: uint16_t(), [open_opt()]) -> {ok, fd()} | {error, posix()}.
open(Port, Options) when is_integer(Port), is_list(Options) ->
    {Tmpdir, Pipe} = make_unix_socket_path(Options),
    {ok, FD} = fdopen(Pipe),

    Cmd = getopts(
        [
            {port, Port},
            {pipe, Pipe}
        ] ++ Options
    ),

    Socket = exec(FD, Cmd),
    _ = close(FD),

    cleanup_unix_socket(Tmpdir, Pipe),
    Socket.

% Run the setuid helper
exec(FD, Cmd) ->
    exec(FD, Cmd, {error, enoent}).

exec(_FD, [], Errno) ->
    Errno;
exec(FD, [Cmd | Rest], _LastErrno) ->
    Proc = open_port({spawn, Cmd}, [exit_status]),
    ExitValue =
        receive
            {Proc, {exit_status, 0}} ->
                ok;
            {Proc, {exit_status, 127}} ->
                {error, enoent};
            {Proc, {exit_status, Status}} ->
                {error, errno_id(Status)}
        end,

    case ExitValue of
        ok ->
            fdget(FD);
        {error, N} = Errno when N =:= eacces; N =:= enoent; N =:= eperm ->
            exec(FD, Rest, Errno);
        Errno ->
            Errno
    end.

% Unix socket handling: retrieves the fd from the setuid helper
make_unix_socket_path(Options) ->
    {Tmpdir, Socket} =
        case proplists:get_value(pipe, Options) of
            undefined ->
                Tmp = procket_mktmp:dirname(),
                ok = procket_mktmp:make_dir(Tmp),
                Path = Tmp ++ "/sock",
                {Tmp, Path};
            Path ->
                {false, Path}
        end,
    {Tmpdir, Socket}.

cleanup_unix_socket(false, Pipe) ->
    prim_file:delete(Pipe);
cleanup_unix_socket(Tmpdir, Pipe) ->
    prim_file:delete(Pipe),
    procket_mktmp:close(Tmpdir).

fdopen(Path) when is_list(Path) ->
    fdopen(list_to_binary(Path));
fdopen(Path) when is_binary(Path), byte_size(Path) < ?UNIX_PATH_MAX ->
    {ok, Socket} = socket(family(local), type(stream), 0),
    Len = byte_size(Path),
    Sun = <<
        (sockaddr_common(family(local), Len))/binary, Path/binary, 0:((unix_path_max() - Len) * 8)
    >>,
    ok = bind(Socket, Sun),
    ok = listen(Socket, 0),
    {ok, Socket}.

fdget(Socket) ->
    {ok, S} = accept(Socket),
    fdrecv(S).

% @private
% Construct the cli arguments for the helper
getopts(Options) ->
    Exec = proplists:get_value(exec, Options, ["", "sudo"]),
    Progname = proplists:get_value(progname, Options, progname()),

    Args = join([optarg(Arg) || Arg <- Options]),
    Redirect = "> /dev/null 2>&1",

    [join([E, Progname, Args, Redirect]) || E <- Exec].

join(StringList) ->
    string:join([N || N <- StringList, N =/= ""], " ").

optarg({backlog, Arg}) ->
    switch("b", Arg);
optarg({pipe, Arg}) ->
    switch("u", Arg);
optarg({protocol, Proto}) when is_atom(Proto) ->
    optarg({protocol, protocol(Proto)});
optarg({protocol, Proto}) when is_integer(Proto) ->
    switch("P", Proto);
optarg({type, Type}) when is_atom(Type) ->
    optarg({type, type(Type)});
optarg({type, Type}) when is_integer(Type) ->
    switch("T", Type);
optarg({family, Family}) when is_atom(Family) ->
    optarg({family, family(Family)});
optarg({family, Family}) when is_integer(Family) ->
    switch("F", Family);
optarg({ip, Arg}) when is_tuple(Arg) -> inet_parse:ntoa(Arg);
optarg({ip, Arg}) when is_list(Arg) -> Arg;
optarg({port, Port}) when is_integer(Port) ->
    switch("p", Port);
optarg({interface, Name}) when is_list(Name) ->
    case is_interface(Name) of
        true ->
            switch("I", Name);
        false ->
            erlang:error(badarg, [{interface, Name}])
    end;
optarg({dev, Dev}) when is_list(Dev) ->
    case is_device(Dev) of
        true ->
            switch("d", Dev);
        false ->
            erlang:error(badarg, [{dev, Dev}])
    end;
optarg({namespace, NS}) when is_list(NS) ->
    switch("N", NS);
% Ignore any other arguments
optarg(_Arg) ->
    "".

switch(Switch, Arg) ->
    lists:concat(["-", Switch, " ", Arg]).

is_interface(Name) when is_list(Name) ->
    % An interface name is expected to consist of a reasonable
    % subset of all characters, use a whitelist and extend it if needed
    Name ==
        [
            C
         || C <- Name,
            (((C bor 32) >= $a) and ((C bor 32) =< $z)) or
                ((C >= $0) and (C =< $9)) or (C == $.)
        ].

is_device(Name) when is_list(Name) ->
    Name ==
        [
            C
         || C <- Name,
            ((C >= $a) and (C =< $z)) or
                ((C >= $0) and (C =< $9) or (C == $/))
        ].

progname_ebin() ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        ?MODULE
    ]).

progname_priv() ->
    case application:get_env(?MODULE, port_executable) of
        {ok, Executable} ->
            Executable;
        undefined ->
            filename:join([
                code:priv_dir(?MODULE),
                ?MODULE
            ])
    end.

% @private
progname() ->
    % Is there a proper way of getting App-Name in this context?
    case code:priv_dir(?MODULE) of
        {error, bad_name} -> progname_ebin();
        _ -> progname_priv()
    end.

% @doc Convert protocol family (aka domain) to OS defined constant.
-spec family(Protocol :: family()) -> 0 | 1 | 2 | 10 | 16 | 17 | 24 | 26 | 28 | 30 | 38.
family(unspec) ->
    0;
family(inet) ->
    2;
family(inet6) ->
    case os:type() of
        {unix, linux} -> 10;
        {unix, darwin} -> 30;
        {unix, freebsd} -> 28;
        {unix, netbsd} -> 24;
        {unix, openbsd} -> 24;
        {unix, sunos} -> 26
    end;
family(netlink) ->
    case os:type() of
        {unix, linux} -> 16;
        {unix, freebsd} -> 38
    end;
family(packet) ->
    17;
family(Proto) when Proto == local; Proto == unix; Proto == file -> 1.

%% Socket type
type(stream) ->
    case os:type() of
        {unix, sunos} -> 2;
        {unix, _} -> 1
    end;
type(dgram) ->
    case os:type() of
        {unix, sunos} -> 1;
        {unix, _} -> 2
    end;
type(raw) ->
    case os:type() of
        {unix, sunos} -> 4;
        {unix, _} -> 3
    end;
type(seqpacket) ->
    5.

% Select a protocol within the family (0 means use the default
% protocol in the family)
protocol(ip) -> 0;
protocol(icmp) -> 1;
protocol(tcp) -> 6;
protocol(udp) -> 17;
protocol(ipv6) -> 41;
protocol(icmp6) -> 58;
protocol('ipv6-icmp') -> 58;
protocol(raw) -> 255.

maybe_atom(_Type, Value) when is_integer(Value) -> Value;
maybe_atom(family, Value) -> family(Value);
maybe_atom(type, Value) -> type(Value);
maybe_atom(protocol, Value) -> protocol(Value).

%%
%% Portability
%%

% struct sockaddr
sockaddr_common(Family0, Length) ->
    Family = maybe_atom(family, Family0),
    case erlang:system_info(os_type) of
        {unix, BSD} when
            BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd
        ->
            <<Length:8, Family:8>>;
        {unix, _} ->
            <<Family:16/native>>
    end.

% UNIX_PATH_MAX
unix_path_max() ->
    case erlang:system_info(os_type) of
        {unix, BSD} when
            BSD == darwin;
            BSD == openbsd;
            BSD == netbsd;
            BSD == freebsd
        ->
            104;
        {unix, _} ->
            108
    end.

% @doc Convert a long unsigned integer from network byte order to host byte order.
%
% == Examples ==
%
% ```
% 1> procket:ntohl(<<1:32>>).
% 16777216
% '''
-spec ntohl(Int :: binary() | uint32_t()) -> uint32_t().
ntohl(<<I:32>>) ->
    ntohl(I);
ntohl(I) when is_integer(I) ->
    <<N:32>> = <<I:32/native>>,
    N.

% @doc Convert a short unsigned integer from network byte order to host byte order.
%
% == Examples ==
%
% ```
% 1> procket:ntohs(<<1:32>>).
% 256
% '''
-spec ntohs(Int :: binary() | uint16_t()) -> uint16_t().
ntohs(<<I:32>>) ->
    ntohs(I);
ntohs(I) when is_integer(I) ->
    <<N:16>> = <<I:16/native>>,
    N.

% @private
wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
% @private
wordalign(Offset, Align) ->
    (Align - (Offset rem Align)) rem Align.
