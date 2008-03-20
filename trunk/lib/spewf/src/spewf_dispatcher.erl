%% @author partdavid@gmail.com
%% @doc This server is the dispatcher. It receives requests for existing
%% sessions, which it looks up in its table and half-proxies. It receives
%% requests for new sessions, for which it generates a session Id and
%% tells its supervisor to start up, passing it the initial request.
%% @end
%%
%% Copyright 2008 partdavid at gmail.com
%%
%% This file is part of SPEWF.
%%
%% SPEWF is free software: you can redistribute it and/or modify it under the
%% terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% SPEWF is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%% FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
%% more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with SPEWF.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(spewf_dispatcher).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% TODO: Make configurable
-define(default_session_timeout, 1000).

%% API
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

%% @spec start_link() -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Starts the dispatcher.
start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @spec stop() -> ok
%% @doc Stops the dispatcher.
stop() ->
    gen_server:cast(?SERVER, stop).

init([]) ->
   process_flag(trap_exit, true),
   Table = ets:new(spewf_sessiontab, [set]),
   {ok, Table}.

handle_call(Request, From, Table) ->
   {reply, {what_is_that, Request, From, Table}, Table}.

handle_cast({request, Mod, From, {Node, new}, Req}, Table) ->
   Ref = make_ref(),
   {ok, Pid} = spewf_dispatch_sup:start_session(Mod, From, {Node, Ref}),
   link(Pid),
   ets:insert(Table, {Ref, Pid}),
   handle_cast({request, Mod, From, {Node, Ref}, Req}, Table);
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

%% @spec request(Module::atom(), sid(), request(), Timeout) ->
%%    {answer, NewSid, Answer} + {error, Reason}
%%    Timeout = integer()
%%    NewSid = sid()
%%    Answer = term()
%%    Reason = term()
%% @doc Send request to appropriate session and receive reply. The dispatcher
%% identified in the sid (session id) dispatches the request to the session
%% which sends the reply directly to the caller.
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

%% request(Module::atom(), sid(), request()) ->
%% @doc As {@link request/4}, with the default session timeout.
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
