-module(boardpub).

-export([out/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include("board.hrl").

out(A) ->
   board:yaws_out(A).
