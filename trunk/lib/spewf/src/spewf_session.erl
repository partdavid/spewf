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
-module(spewf_session).
%% @doc This is the session module. This is the gen_server that the dispatcher
%% communicates with. The gen_server behavior is not quite generic here,
%% the session replies directly back to the calling process rather than
%% the dispatcher.
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-record(session, {sid,
                  module,
                  state}).

%% API
-export([start_link/2,
         start_link/4,
         stop/1]).

-export([request/2, request/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% This module is a behavior module for Spewf-compliant webapp modules
%% An example 'webecho' is included.
-export([behaviour_info/1]).
behaviour_info(callbacks) ->
   [{init, 1},
    {handle_request, 2}];
behaviour_info(_) ->
   undefined.

%% @spec start_link(Module::atom(), sid()) -> {ok, pid()} + {error, Reason}
%% @doc Starts a session.
start_link(Mod, Sid) ->
   gen_server:start_link(?MODULE, [Mod, Sid], []).

%% @spec start_link(Module::atom(), From::pid(), sid(), request()) ->
%% @doc Starts a session, with an initial request.
start_link(Mod, From, Sid, Req) ->
   gen_server:start_link(?MODULE, [Mod, From, Sid, Req], []).

init([Mod, Sid]) ->
   {ok, State, Timeout} = case Mod:init([]) of
                                 {ok, I} -> {ok, I, infinity};
                                 {ok, I, T} -> {ok, I, T}
                              end,
   {ok, #session{module = Mod, state = State, sid = Sid}, Timeout};
init([Mod, From, Sid, Req]) ->
   case catch Mod:init([]) of
      {ok, InitState} -> 
         request(self(), From, Req),
         {ok, #session{module = Mod, state = InitState, sid = Sid}, infinity};
      {ok, InitState, T} -> 
         request(self(), From, Req),
         {ok, #session{module = Mod, state = InitState, sid = Sid}, T};
      Err ->
         {stop, {error_initing_callback_mod, Mod, Err}}
   end.

handle_call(Request, From, State) ->
   {reply, {what_is_that, Request, From, State}, State}.

handle_cast(stop, State) ->
   {stop, normal, State};
handle_cast({request, From, Req}, State) ->
   %% We send our replies directly
   Mod = State#session.module,
   Sid = State#session.sid,
   {Answer, Newstate, Timeout} = 
      case Mod:handle_request(Req, State#session.state) of
         {A, S} -> {A, S, infinity};
         {A, S, T} -> {A, S, T}
      end,
   From ! {answer, Sid, Answer},
   {noreply, #session{module = Mod, sid = Sid, state = Newstate}, Timeout};
handle_cast(_, State) ->
   {noreply, State}.

handle_info(_, State) ->
   {noreply, State}.

terminate(_, _) ->
    ok.

stop(Pid) ->
   gen_server:cast(Pid, stop).

%% For debugging or something
request(Pid, Req) ->
   request(Pid, self(), Req).
   
%% @spec request(Session::pid(), From::pid(), request()) -> ok
%% @doc Intended to be called from the spewf_dispatcher. Makes a
%% request of the session in the given pid. The session arranges
%% to reply directly to the From pid.
request(Pid, From, Req) ->
   gen_server:cast(Pid, {request, From, Req}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% <hr width="80%" style="color: gray" />

%% @spec Module:init(Args) -> {ok, State} + {ok, State, Timeout} + Error
%%    Args = []
%%    State = any()
%%    Timeout = integer()
%% @doc A callback module using the spewf_session behavior implements
%% this callback to perform needed initialization. State and Timeout
%% in the return value have the same meaning as they do for gen_server.
%% @end
%% @spec Module:handle_request(request(), State) ->
%%    {Reply, NewState} + {Reply, NewState, Timeout}
%%    State = any()
%%    NewState = any()
%%    Reply = any()
%%    Timeout = integer()
%% @doc A callback module using the spewf_session behavior implements
%% this callback to handle a request. The meaning of State, NewState
%% and Timeout are the same as they are for a gen_server. Note that
%% a spewf_session does not have the option of not providing a reply.
%% @end

reap_answer(Sid) ->
   receive
      {answer, Sid, Answer} -> Answer
   after 500 ->
         {error, timeout}
   end.

basic_test_() ->
   Sid = {node(), make_ref()},
   {ok, Pid} = start_link(webecho, Sid),
   [
    fun() ->
          request(Pid, self(), [{said, "m1"}]),
          {ehtml, _} = reap_answer(Sid)
    end,
    fun() ->
          ok = stop(Pid)
    end
   ].

mfsr_test_() ->
   Sid = {node(), make_ref()},
   {ok, _Pid} = start_link(webecho, self(), Sid, [{said, "mfsr1"}]),
   [
    ?_assertMatch({ehtml, _}, reap_answer(Sid))
   ].
