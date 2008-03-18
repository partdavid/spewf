%% @doc  
%% @end
-module(bd_app).

-behaviour(application).

-include("board.hrl").

-export([
         start/0,
         start/2,
         shutdown/0,
         stop/1
        ]).

%% @spec start() -> {ok, pid()} + {error, Reason}
%%    Reason = any()
%% @doc Start the board application.
start() ->
   application:start(board).

%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @doc Application behavior callback to starts the board SPEWF application.
start(_Type, StartArgs) ->
   case bd_sup:start_link(StartArgs) of
      {ok, Pid} -> 
         {ok, Pid};
      Error ->
         Error
   end.

%% @spec shutdown() -> ok 
%% @doc Called to shudown the board application.
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    application:stop(board).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Called upon the termintion of an application.
%%--------------------------------------------------------------------
stop(State) ->
    ok.
