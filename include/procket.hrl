%% Copyright (c) 2010-2013, Michael Santos <michael.santos@gmail.com>
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

-define(UNIX_PATH_MAX, 108).
-define(BACKLOG, 128).

-define(IFNAMSIZ, 16).      % Max length of an interface device name including NULL

-define(SOCK_STREAM, 1).    % Sequenced, reliable, connection-based byte streams.
-define(SOCK_DGRAM, 2).     % Connectionless, unreliable datagrams of fixed maximum length.
-define(SOCK_RAW, 3).       % Raw protocol interface.
%-define(SOCK_RDM, 4).       % Reliably-delivered messages.
%-define(SOCK_SEQPACKET,5).  % Sequenced, reliable, connection-based, datagrams of fixed maximum length.
%-define(SOCK_DCCP, 6).      % Datagram Congestion Control Protocol.
%-define(SOCK_PACKET, 10).   % Linux specific way of getting packets at the dev level.
                            % For writing rarp and other similar things on the user level.

% Protocol families
-ifndef(PF_UNSPEC).
-define(PF_UNSPEC,0).                       % Unspecified.
-endif.
-ifndef(PF_LOCAL).
-define(PF_LOCAL, 1).                       % Local to host (pipes and file-domain).
-endif.
-ifndef(PF_UNIX).
-define(PF_UNIX, ?PF_LOCAL).                % POSIX name for PF_LOCAL.
-endif.
-ifndef(PF_INET).
-define(PF_INET, 2).                        % IP protocol family.
-endif.
-ifndef(PF_INET6).
-define(PF_INET6, 10).                      % IP version 6.
-endif.
-ifndef(PF_PACKET).
-define(PF_PACKET, 17).                     % Packet family.
-endif.


-define(SOL_SOCKET, 1).
-define(SO_REUSEADDR, 2).

