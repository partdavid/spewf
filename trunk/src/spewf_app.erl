%% @doc  
-module(spewf_app).

-behaviour(application).

-export([
	 start/2,
	 shutdown/0,
	 stop/1
	 ]).

%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% Look for start_yaws in application config
start(Type, StartArgs) ->
   case application:get_env(spewf, start_yaws) of
      {ok, {DocRoot, ServerConf, GlobalConf}} ->
         case yaws:start_embedded(DocRoot, add_spewf(ServerConf), GlobalConf) of
            ok ->
               start_self(Type, StartArgs);
            Err ->
               {error, {yaws_error, Err}}
         end;
      _ ->
         %% We're not starting yaws
         start_self(Type, StartArgs)
   end.
         
add_spewf(ServerConf) ->
   case lists:keysearch(appmods, 1, ServerConf) of
      {value, {appmods, Appmods}} ->
         lists:keystore(appmods, 1, ServerConf, {appmods, [spewf|Appmods]});
      false ->
         lists:keystore(appmods, 1, ServerConf, {appmods, [spewf]})
   end.

start_self(_Type, StartArgs) ->
   case spewf_sup:start_link(StartArgs) of
      {ok, Pid} -> 
         {ok, Pid};
      Error ->
         Error
   end.

%%--------------------------------------------------------------------
%% @doc Called to shutdown the spewf application.
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
