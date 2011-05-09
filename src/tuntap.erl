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
-module(tuntap).

-include("ioctl.hrl").
-include("tuntap.hrl").

-export([
        create/0, create/1, create/2,
        persist/2, destroy/1,
        owner/2, group/2
    ]).


create() ->
    create(<<0:(15*8)>>, [tap, no_pi]).
create(Ifname) ->
    create(Ifname, [tap, no_pi]).

create(Ifname, Opt) when is_list(Ifname) ->
    create(list_to_binary(Ifname), Opt);
create(Ifname, Opt) when byte_size(Ifname) < 16, is_list(Opt) ->
    Flags = lists:foldl(fun(N, F) -> F bor flag(N) end, 0, Opt),

    {ok, FD} = procket:dev("net/tun"),
    {ok, Dev} = procket:ioctl(FD, ?TUNSETIFF,
        <<Ifname/binary, 0:((15*8) - (byte_size(Ifname)*8)), 0:8,      % ifrn_name[IFNAMSIZ]: interface name
        Flags:2/native-signed-integer-unit:8,                          % ifru_flags
        0:(14*8)>>),
    {ok, FD, hd(binary:split(Dev, <<0>>))}.

persist(FD, Bool) when is_integer(FD) ->
    ioctl(FD, ?TUNSETPERSIST, bool(Bool)).

destroy(FD) when is_integer(FD) ->
    persist(FD, false).

owner(FD, Owner) when is_integer(FD), is_integer(Owner) ->
    ioctl(FD, ?TUNSETOWNER, int_to_bin(Owner)).

group(FD, Group) when is_integer(FD), is_integer(Group) ->
    ioctl(FD, ?TUNSETGROUP, int_to_bin(Group)).


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
ioctl(FD, Request, Opt) ->
    case procket:ioctl(FD, Request, Opt) of
        {ok, _} -> ok;
        Error -> Error
    end.

flag(tun) -> ?IFF_TUN;
flag(tap) -> ?IFF_TAP;
flag(no_pi) -> ?IFF_NO_PI;
flag(one_queue) -> ?IFF_ONE_QUEUE;
flag(vnet_hdr) -> ?IFF_VNET_HDR;
flag(tun_excl) -> ?IFF_TUN_EXCL.

int_to_bin(Int) ->
    <<Int:4/native-integer-unsigned-unit:8>>.

bool(true) -> <<1:4/native-integer-unsigned-unit:8>>;
bool(false) -> <<0:4/native-integer-unsigned-unit:8>>.
