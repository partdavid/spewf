%% @doc This is the session module. This is the gen_server that the dispatcher
%% communicates with. The gen_server behavior is not quite generic here,
%% the session replies directly back to the calling process rather than
%% the dispatcher.
-module(spewf_session).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-record(session, {sid,
                  module,
                  state}).

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

%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
start_link(Mod, Sid) ->
   gen_server:start_link(?MODULE, [Mod, Sid], []).

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
   
%% @doc Really only called from dispatcher, that's why explicit From
request(Pid, From, Req) ->
   gen_server:cast(Pid, {request, From, Req}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
