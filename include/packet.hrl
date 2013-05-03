%% Copyright (c) 2012-2013, Michael Santos <michael.santos@gmail.com>
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
-define(SIOCGIFINDEX, 16#8933).
-define(PF_PACKET, 17).

% Options for retrieving device IP
-define(SIOCGIFADDR, 16#8915).
-define(PF_INET, 2).

% Options for retrieving dev MAC address
-define(SIOCGIFHWADDR, 16#8927).

% Options for promiscuous mode
-define(SOL_PACKET, 263).
-define(PACKET_ADD_MEMBERSHIP, 1).
-define(PACKET_DROP_MEMBERSHIP, 2).
-define(PACKET_MR_PROMISC, 1).

% Options for binding to interfaces
-define(SOL_SOCKET, 1).
-define(SO_BINDTODEVICE, 25).

% Options for BPF filtering
-define(SO_ATTACH_FILTER, 26).
-define(SO_DETACH_FILTER, 27).

-define(ETH_P_IP, 16#0800).
