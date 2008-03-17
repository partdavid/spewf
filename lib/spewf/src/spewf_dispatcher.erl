%% @doc This server is the dispatcher. It receives requests for existing
%% sessions, which it looks up in its table and half-proxies. It receives
%% requests for new sessions, for which it generates a session Id and
%% tells its supervisor to start up, passing it the initial request.
-module(spewf_dispatcher).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-define(default_session_timeout, 1000).

-export([start_link/0,
         stop/0]).

-export([request/3, request/4]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%====================================================================
%% Server functions
%%====================================================================

init([]) ->
   process_flag(trap_exit, true),
   Table = ets:new(spewf_sessiontab, [set]),
   {ok, Table}.

handle_call(Request, From, Table) ->
   {reply, {what_is_that, Request, From, Table}, Table}.

handle_cast({request, Mod, From, {Node, new}, Req}, Table) ->
   Ref = make_ref(),
   {ok, Pid} = spewf_dispatch_sup:start_session(Mod, From, {Node, Ref}, Req),
   link(Pid),
   ets:insert(Table, {Ref, Pid}),
   {noreply, Table};
handle_cast({request, Mod, From, {Node, Ref}, Req}, Table) ->
   case ets:lookup(Table, Ref) of
      [{Ref, Pid}|_] ->
         spewf_session:request(Pid, From, Req);
      [] ->
         handle_cast({request, Mod, From, {Node, new}, Req}, Table)
   end,
   {noreply, Table};
handle_cast(stop, Table) ->
   {stop, normal, Table};
handle_cast(_Msg, Table) ->
   {noreply, Table}.

handle_info({'EXIT', Pid, _Reason}, Table) ->
   %% Delete session from table
   case ets:match(Table, {'_', Pid}) of
      [{Ref, Pid}] ->
         ets:delete(Table, {Ref, Pid});
      _ -> ok
   end,
   {noreply, Table};
handle_info(_Info, Table) ->
   {noreply, Table}.

%% More than a little funky. This is where I break the gen_server
%% communication abstraction because of the asymmetric message-sending
request(Mod, {local, Ref}, Req, Timeout) ->
   gen_server:cast(?SERVER, {request, Mod, self(), {node(), Ref}, Req}),
   receive
      {answer, {_, NewRef}, Answer} ->
         {answer, {local, NewRef}, Answer}
   after Timeout ->
         {error, timeout, Timeout, {local, Ref}}
   end;
request(Mod, {Node, Ref}, Req, Timeout) ->
   gen_server:cast({?SERVER, Node}, {request, Mod, self(), {Node, Ref}, Req}),
   receive
      {answer, {Node, NewRef}, Answer} ->
         {answer, {Node, NewRef}, Answer}
   after Timeout ->
         {error, timeout, Timeout, {Node, Ref}}
   end.

request(Mod, Sid, Req) ->
   request(Mod, Sid, Req, ?default_session_timeout).

terminate(_Reason, Table) ->
   ets:delete(Table),
   ok.

code_change(_OldVsn, Table, _Extra) ->
   {ok, Table}.

%% Tests
basic_test_() ->
   [
    ?_assertMatch({ok, _Pid}, start_link()),
    ?_assertMatch(ok, stop())
   ].
