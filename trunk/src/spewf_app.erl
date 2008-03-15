%% @doc  
-module(spewf_app).

-behaviour(application).

-export([
	 start/2,
	 shutdown/0,
	 stop/1
	 ]).

%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
start(_Type, StartArgs) ->
   case spewf_sup:start_link(StartArgs) of
      {ok, Pid} -> 
         {ok, Pid};
      Error ->
         Error
   end.

%%--------------------------------------------------------------------
%% @doc Called to shudown the spewf application.
%% @spec shutdown() -> ok 
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    application:stop(spewf).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Called upon the termintion of an application.
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
