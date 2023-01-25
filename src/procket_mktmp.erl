%%% @copyright 2010-2023 Michael Santos <michael.santos@gmail.com>
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

%% Atomically create a temporary file.
%%
%% This module just creates a directory, put whatever temp files
%% you need inside the directory.
%%
-module(procket_mktmp).

-export([dirname/0, name/1, template/2, make_dir/1, close/1]).

-include_lib("kernel/include/file.hrl").

-define(TEMPLATE, "erlang-XXXXXXXXXXXX").
-define(S_IRWXU, 8#00400 bor 8#00200 bor 8#00100).
-define(ALPHANUM,
    "0123456789"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
).

dirname() ->
    TMP =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    name(TMP ++ "/" ++ ?TEMPLATE).

name(Template) ->
    template(Template, ?ALPHANUM).

template(Name, Chars) ->
    template(lists:reverse(Name), [], Chars).
template([$X | Rest], Acc, Chars) ->
    template(
        Rest,
        [lists:nth(rand:uniform(length(Chars)), Chars) | Acc],
        Chars
    );
template(Name, Rand, _) when length(Rand) >= 6 ->
    lists:reverse(Name) ++ Rand.

make_dir(Path) ->
    case prim_file:make_dir(Path) of
        ok ->
            prim_file:write_file_info(Path, #file_info{mode = ?S_IRWXU});
        Error ->
            Error
    end.

close(Path) ->
    prim_file:del_dir(Path).
