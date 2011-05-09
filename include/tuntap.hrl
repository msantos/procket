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

-define(SIZEOF_INT, 4).
-define(SIZEOF_UNSIGNED_INT, ?SIZEOF_INT).

% Ioctl defines
-define(TUNSETNOCSUM, ?IOW($T, 200, ?SIZEOF_INT)).
-define(TUNSETDEBUG, ?IOW($T, 201, ?SIZEOF_INT)).
-define(TUNSETIFF, ?IOW($T, 202, ?SIZEOF_INT)).
-define(TUNSETPERSIST, ?IOW($T, 203, ?SIZEOF_INT)).
-define(TUNSETOWNER, ?IOW($T, 204, ?SIZEOF_INT)).
-define(TUNSETLINK, ?IOW($T, 205, ?SIZEOF_INT)).
-define(TUNSETGROUP, ?IOW($T, 206, ?SIZEOF_INT)).
-define(TUNGETFEATURES, ?IOR($T, 207, ?SIZEOF_UNSIGNED_INT)).
-define(TUNSETOFFLOAD, ?IOW($T, 208, ?SIZEOF_UNSIGNED_INT)).
-define(TUNSETTXFILTER, ?IOW($T, 209, ?SIZEOF_UNSIGNED_INT)).
-define(TUNGETIFF, ?IOR($T, 210, ?SIZEOF_UNSIGNED_INT)).
-define(TUNGETSNDBUF, ?IOR($T, 211, ?SIZEOF_INT)).
-define(TUNSETSNDBUF, ?IOW($T, 212, ?SIZEOF_INT)).

% TUNSETIFF ifr flags
-define(IFF_TUN, 16#0001).
-define(IFF_TAP, 16#0002).
-define(IFF_NO_PI, 16#1000).
-define(IFF_ONE_QUEUE, 16#2000).
-define(IFF_VNET_HDR, 16#4000).
-define(IFF_TUN_EXCL, 16#8000).
