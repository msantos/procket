
procket is an Erlang library for socket creation and manipulation.

procket can use a setuid helper so actions like binding low ports and
requesting some sockets types can be done while Erlang is running as an
unprivileged user.

## FEATURES

Other features include:

* low level socket manipulation using socket/3, ioctl/3, setsockopt/4, ...

* support any protocols supported by the socket interface: ICMP, Unix
  sockets, ...

* support for the BSD raw socket interface

* generate and snoop packets using PF_PACKET sockets on Linux

* generate and snoop packets using the BPF interface on BSDs like Mac OS X

* support for creating and reading/writing from character devices like
  TUN/TAP interfaces


## REQUIREMENTS

procket works with any version of Erlang after R14A.

## CHANGES

### V0.04:
* IPv6 support

### V0.02:
* procket:listen/1,2 was renamed procket:open/1,2. procket:listen/2 is
  now a wrapper around listen(2)

* procket:recvfrom/2 returns {error,eagain} when data is not available
  (previously returned nodata)


## EXPORTS

### DATA TYPES

    protocol() = ip | icmp | tcp | udp | 'ipv6-icmp' | raw

    type() = stream | dgram | raw

    family() = unspec | inet | inet6 | netlink | packet


### Accessing Socket/Devices Requiring Elevated Privileges

    open(Port) -> {ok, FD} | {error, posix()}
    open(Port, Options) -> {ok, FD} | {error, posix()}

        Types   Port = 0..65535
                Options = [Opts]
                Opts = {protocol, Protocol} | {type, Type} | {family, Family}
                    | {ip, IPAddress}
                    | {dev, string()}
                    | {exec, [string()]}
                    | {progname, string()}
                    | {interface, string()}
                    | {pipe, string()}
                Protocol = protocol() | integer()
                Type = type() | integer()
                Family = family() | integer()
                IPAddress = inet:ip_address()
                FD = integer()

        Open a socket or device using the procket setuid helper. The
        file descriptor is passed back over a Unix socket.

        The default behaviour of open/1,2 is to attempt to open the
        socket twice: first by running the procket setuid helper and, if this
        fails because the process does not have the appropriate permissions,
        running the setuid helper again using "sudo". The default behaviour
        can be changed by using the 'exec' option:

            procket:open(Port, [{exec, ["", "sudo"]}]).

    dev(Dev) -> {ok, FD} | {error, posix()}

        Types   Dev = string()

        Wrapper around open/2. Opens a character device such as bpf,
        tun or tap devices.


### BSD Socket Interface

    socket(Family, Type, Protocol) -> {ok, FD} | {error, posix()}

        Types   Family = family() | integer()
                Type = type() | integer()

        See socket(2).

    listen(Socket) -> ok | {error, posix()}
    listen(Socket, Backlog) -> ok | {error, posix()}

        Types   Socket = integer()
                Backlog = integer()

        See listen(2). listen/1 sets the backlog to 50.

    connect(Socket, Sockaddr) -> ok | {error, posix()}

        Types   Socket = integer()
                Sockaddr = <<>> | binary()

        See connect(2).

        Sockaddr is a struct sockaddr whose layout is dependent on
        platform. If Sockaddr is an empty binary, connect(2) will be
        called with NULL as the second option.

    accept(Socket) -> {ok, FD} | {error, posix()}
    accept(Socket, Salen) -> {ok, FD, Sockaddr} | {error, posix()}

        Types   Socket = integer()
                Salen = 0 | non_neg_integer()
                Sockaddr = binary()

        See accept(2).

        accept/1 returns the file descriptor associated with the new
        connection.

        accept/2 will allocate a struct sockaddr of size Salen bytes
        that will hold the peer address. If the size is too small, the
        returned binary will be zero padded to indicate the size required.

    close(Socket) -> ok | {error, posix()}

        Types   Socket = integer()

        See close(2).

    recv(Socket, Size) -> {ok, Buf} | {error, posix()}
    recvfrom(Socket, Size) -> {ok, Buf} | {error, posix()}
    recvfrom(Socket, Size, Flags, Salen) -> {ok, Buf, Sockaddr}

        Types   Socket = integer()
                Size = ulong()
                Flags = integer()
                Salen = 0 | ulong()
                Buf = binary()
                Sockaddr = binary()

        See recv(2).

    recvmsg(Socket, Size, Flags, CtrlDataSize) -> {ok, Buf, Flags, CtrlData} |
                                                  {error, posix()}
    recvmsg(Socket, Size, Flags, CtrlDataSize, SockaddrSize) -> {ok, Buf, Flags, CtrlData, Sockaddr} |
                                                  {error, posix()}

        Types   Socket = integer()
                Size = ulong()
                CtrlDataSize = ulong()
                SockaddrSize = ulong()
                Flags = integer()
                Buf = binary()
                Sockaddr = binary()
                CtrlData = [{integer(), integer(), binary()}]

        See recvmsg(2) and cmsg(3).

        The control data, if any, is returned as a list of 3-tuples consisting of the cmsg
        level, type and data fields.

    sendto(Socket, Buf) -> ok | {error, posix()}
    sendto(Socket, Buf, Flags) -> ok | {error, posix()}
    sendto(Socket, Buf, Flags, Sockaddr) -> ok | {ok, Size} | {error, posix()}

        Types   Socket = integer()
                Flags = integer()
                Buf = binary()
                Sockaddr = binary()
                Size = non_neg_integer()

        See sendto(2).

        In the case of a partial write, sendto/4 will return the number
        of bytes written.

    sendmsg(Socket, Buf, Flags, CtrlData) -> ok | {error, posix()}
    sendmsg(Socket, Buf, Flags, CtrlData, Sockaddr) -> ok | {ok, Size} | {error, posix()}

        Types   Socket = integer()
                Buf = binary()
                Flags = integer()
                CtrlData = [{integer(), integer(), binary()}]
                Sockaddr = binary()
                Size = non_neg_integer()

        See sendmsg(2) and cmsg(3).

        The control data, if any, is sent as a list of 3-tuples consisting of the cmsg
        level, type and data fields.

        In the case of a partial write, sendmsg/5 will return the number
        of bytes written.

    read(FD, Length) -> {ok, Buf} | {error, posix()}

        Types   FD = integer()
                Length = ulong()
                Buf = binary()

        See read(2).

        The returned byte_size(Buf) is the actual number of bytes read.

    write(FD, Buf) -> ok | {ok, Size} | {error, posix()}
    writev(FD, Bufs) -> ok | {ok, Size} | {error, posix()}

        Types   FD = integer()
                Buf = Bufs | binary()
                Bufs = [ binary() ]
                Size = non_neg_integer()

        See write(2) and writev(2).

        write/2 and writev/2 will return 'ok' if the complete buffer was
        written and {ok,non_neg_integer()} in the case of a partial write:

            write_exact(FD, Buf) ->
                case procket:write(FD, Buf) of
                    ok ->
                        ok;
                    {ok, N} ->
                        <<_:N/bytes, Rest/binary>> = Buf,
                        write_exact(FD, Rest);
                    Error ->
                        Error
                end.

    bind(Socket, Sockaddr) -> ok | {error, posix()}

        Types   Socket = integer()
                Sockaddr = binary()

        See bind(2).

    setsockopt(Socket, Level, Optname, Optval) -> ok | Error

        Types   Socket = integer()
                Level = integer() | atom()
                Optname = integer() | atom()
                Optval = binary()
                Error = {error, posix() | unsupported}

        See setsockopt(2).

        Level and Optname can either be an integer or an atom with the
        same name as the definitions in the system header files, e.g.,
        'IPPROTO_IPIP', 'SO_REUSEPORT'. Note these are uppercase atoms
        and so must be quoted.

        If an atom is used as an argument and is not supported by the OS,
        setsockopt/4 will return {error,unsupported}.

    getsockopt(Socket, Level, Optname, Optval) -> {ok, Buf} | Error

        Types   Socket = integer()
                Level = integer() | atom()
                Optname = integer() | atom()
                Optval = binary()
                Buf = binary()
                Error = {error, posix() | unsupported}

        See getsockopt(2). Similar to inet:getopts/2 but can be used
        with file descriptors.

        Retrieve a socket option for a file descriptor. Use an empty
        binary to indicate no option value is supplied or will be
        returned.

        Also see setsockopt/4.

    ioctl(FD, Request, Arg) -> {ok, Result} | {error, posix()}

        Types   FD = integer()
                Request = ulong()
                Arg = binary() | integer()

        See ioctl(2). Be careful with this function.

        Request is an integer with the direction of the request encoded
        into it (IN, OUT, IN/OUT). Result is a binary holding the result.
        If the ioctl is IN only, the Result will be the same as Arg.

        Arg is a structure dependent on the request.

        See procket_ioctl.erl for some helper functions for dealing
        with ioctl.

        Caveats:
            * Request is an integer on Linux and an unsigned long on OS X

            * some ioctl requests require a structure with a pointer to
              memory. Use alloc/1 to create these structures and buf/1 to
              retrieve the data from them.

            * some ioctl requests use an integer rather a pointer to
              a structure. This means that it's possible to pass in an
              arbitrary pointer (an integer) as an argument to an ioctl
              expecting a structure. Don't do this.

    alloc(Struct) -> {ok, Arg, Resource} | {error, posix()}

        Types   Struct = [ binary | {ptr, Length} | {ptr, binary()} ]
                Arg = binary()
                Length = ulong()
                Resource = [resource()]

        Create a structure containing pointers to memory that can be
        passed as the third argument to ioctl/3.

        The size of the allocated memory can be indicated by either
        using an integer or passing in a binary of the appropriate size.
        If an integer is used, the contents are zero'ed. If a binary is
        used, the memory is initialized to the contents of the binary.

        Resource is a list of NIF resources (one for each piece of
        allocated memory) requested in the struct. The memory will
        automatically be freed by the resource.

        It is up to the caller to ensure the structure has the proper
        endianness and alignment for the platform.

        For example, a struct bpf_program is used to set a filter on a
        bpf character device:

            struct bpf_program {
                u_int bf_len;
                struct bpf_insn *bf_insns;
            };

            struct bpf_insn {
                u_short     code;
                u_char      jt;
                u_char      jf;
                bpf_u_int32 k;
            };

        To allocate a binary in Erlang:

            Insn = [
                ?BPF_STMT(?BPF_LD+?BPF_H+?BPF_ABS, 12),                     % offset = Ethernet Type
                ?BPF_JUMP(?BPF_JMP+?BPF_JEQ+?BPF_K, ?ETHERTYPE_IP, 0, 1),   % type = IP

                ?BPF_STMT(?BPF_RET+?BPF_K, 16#FFFFFFFF),                    % return: entire packet
                ?BPF_STMT(?BPF_RET+?BPF_K, 0)                               % return: drop packet
            ],
            {ok, Code, [Res]} = procket:alloc([
                <<(length(Insn)):4/native-unsigned-integer-unit:8>>,
                {ptr, list_to_binary(Insn)}
            ]).

        To use the ioctl and return the contents of the memory:

            case procket:ioctl(Socket, ?BIOCSETF, Code) of
                {ok, _} ->
                    procket:buf(Res);
                Error ->
                    Error
            end.

    buf(Resource) -> {ok, Buf} | {error, enomem}

        Types   Resource = resource()
                Buf = binary()

        Returns the contents of memory allocted using alloc/1. See the
        example above.

## COMPILING

Try running: make


## SETUID vs SUDO vs Capabilities

The procket helper executable needs root privileges. Either allow your
user to run procket using sudo or copy procket to somewhere owned by
root and make it setuid.

* for sudo

        sudo visudo
        youruser ALL=NOPASSWD: /path/to/procket/priv/procket

        # if sudoers has enabled "Default requiretty", you will need to set
        # one of these options too:

        Defaults!/path/to/procket/priv/procket !requiretty
        Defaults:youruser !requiretty

* to make it setuid

        # Default location
        sudo chown root priv/procket
        sudo chmod 4750 priv/procket

  The procket setuid helper can also be placed in a system directory:

        # System directory
        sudo cp priv/procket /usr/local/bin
        sudo chown root:yourgroup /usr/local/bin/procket
        sudo chmod 4750 /usr/local/bin/procket

  Then pass the progname argument to open/2:

        {ok, FD} = procket:open(53, [{progname, "/usr/local/bin/procket"},
                {protocol, udp},{type, dgram},{family, inet}]).

* use Linux capabilities: beam or the user running beam can be
given whatever socket privileges are needed. For example, using file
capabilities:

        setcap cap_net_raw=ep /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp

    To see the capabilities:

        getcap /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp

    To remove the capabilities:

        setcap -r /usr/local/lib/erlang/erts-5.8.3/bin/beam.smp


## USING IT

    $ erl -pa ebin
    Erlang R13B03 (erts-5.7.4) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.7.4  (abort with ^G)
    1> {ok, FD} = procket:open(53, [{protocol, udp},{type, dgram},{family, inet}]).
    {ok,9}
    2> {ok, S} = gen_udp:open(0, [{fd,FD}]).
    {ok,#Port<0.929>}
    3> receive M -> M end.
    {udp,#Port<0.929>,{127,0,0,1},47483,"hello\n"}
    4>

    $ nc -u localhost 53
    hello
    ^C


## EXAMPLES

To build the examples:

    make examples

### Simple echo server

    $ erl -pa ebin
    1> echo:start(53, [{protocol, tcp}, {type, stream}, {family, inet6}]).

### ICMP ping

    1> icmp:ping("www.yahoo.com").

### Sniff the network

    1> {ok, S} = procket:open(0, [{protocol, 16#0008}, {type, raw}, {family, packet}]).
    {ok,12}
    2> procket:recvfrom(S, 2048).
    {ok,<<0,21,175,89,8,38,0,3,82,3,39,36,8,0,69,0,0,52,242,
              0,0,0,52,6,188,81,209,...>>}
    3> Port = erlang:open_port({fd, S, S}, [binary, stream]).
    4> flush().
    Shell got {#Port<0.1343>,
              {data,<<224,105,149,59,163>>}}

### Bind to one or more interfaces

    1> procket:open(53, [{progname, "sudo priv/procket"},{protocol, udp},{type,dgram},{interface, "br0"}]).
    {ok,9}
    2> procket:open(53, [{progname, "sudo priv/procket"},{protocol, udp},{type,dgram},{interface, "br1"}]).
    {ok,10}


## HOW IT WORKS

procket creates a local domain socket and spawns a small setuid binary
(or runs it under sudo). The executable opens a socket, drops privs and
passes the file descriptor back to Erlang over the Unix socket.

procket uses libancillary for passing file descriptors between processes:

    http://www.normalesup.org/~george/comp/libancillary/


## TODO

* Docs and type specs

* Try to re-use the Unix socket when requesting more fd's from the procket
  executable

* Make a procket gen\_server (gen\_raw(?)).
    * Support passive and active modes.
    * Hold state for the socket, so the caller does not need to, e.g.,
      use ifindex/2.
    * same interface for PF_PACKET and BPF


## CONTRIBUTORS

### Magnus Klaar
* support for binding a socket to an interface
* Makefile fixes

### Gregory Haskins
* fix link-error on SUSE platforms
* socket notifications
* writev support

### Roman Gafiyatullin
* support running from an OTP app and compressed bundles

### Kenji Rikitake

* Added ICMPv6 support (preliminary, buggy on FreeBSD localhost I/F)

    Many localhost interfaces do not respond ICMP Echo Requests with
    the proper Echo Reply code.

* Tested on FreeBSD/amd64 9.0-RELEASE alc0 interface driver

### YAMAMOTO Takashi

* BPF support for NetBSD

* sudo fallback for setuid helper
