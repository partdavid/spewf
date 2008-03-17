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
-module(spewf_server).
%% @doc This server is the "session manager". It handles the round-robin
%% handing out of session specifications, which are incomplete child
%% specifications. In its simplest form, 
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, start_shell/0,
         stop/0]).

-export([next/1, register_subapp/2, unregister_subapp/1, info/0, info/1,
        append_to_dispatcher_list/2, remove_from_dispatcher_list/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(sessionspec, {next=[], done=[], last=undefined}).

-define(SERVER, ?MODULE).

%% @spec start_link() -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Starts the session management server.
start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec start_shell() -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Starts the session management server, without linking.
start_shell() ->
   {ok, Pid} = start_link(),
   unlink(Pid),
   {ok, Pid}.

%% @spec stop() -> ok
%% @doc Stops the session management server.
stop() ->
    gen_server:cast(?SERVER, stop).

%% @spec next(Module::atom()) -> Node
%%    Node = local + atom()
%% @doc Gets the node at which the next session for the given subapp module
%% should be created.
next(Mod) ->
   gen_server:call(?SERVER, {next, Mod}).

%% @spec register_subapp(Module, DispatcherList) ->
%%    {registered, Module} + {error, Reason}
%%    Module = atom()
%%    Reason = term()
%%    DispatcherList = [Dispatcher]
%%    Dispatcher = Node + function()
%%    Node = atom()
%% @doc Register the subapp given by module with the initial list of
%% available dispatchers given by DispatcherList. The dispatcher list
%% may contain nodenames or functions, as with
%% {@link append_to_dispatcher_list/2}.
register_subapp(Mod, DispatcherList) ->
   gen_server:call(?SERVER, {register_subapp, Mod, DispatcherList}).

%% @spec unregister_subapp(Module::atom()) -> ok + {error, Reason}
%%    Reason = term()
%% @doc Remove the subapp from the session manager. Any further requests
%% via {@link next/1} for the subapp will use the ``spewf'' built-in
%% default.
unregister_subapp(Mod) ->
   gen_server:call(?SERVER, {unregister_subapp, Mod}).

%% @spec append_to_dispatcher_list(Module::atom(), Dispatcher) ->
%%    ok + {error, Reason}
%%    Reason = term()
%%    Dispatcher = Node + function()
%%    Node = atom()
%% @doc Appends the dispatcher specification to the end of the dispatcher
%% list for the given subapp. The dispatcher specification may be
%% a nodename or a fun. If a nodename, then that nodename is returned
%% for {@link next/1} calls that occur when that element is at the head
%% of the dispatcher list.
%%
%% When the element at the head of the dispatcher list is a fun of
%% arity 0, that fun is evaluated and expected to provide a nodename,
%% which is returned as the value of {@link next/1}. When the element
%% at the head of the dispatcher list is a fun of arity 1, the fun
%% is passed the last return value of {@link next/1} and the value it
%% returns is returned as the value of {@link next/1}. If no nodenames
%% have been returned, the fun receives the atom ``undefined''.
%%
%% @todo A fun of arity 2 would receive the last dispatcher and the
%% current request (requires modifying next/1 and the spewf appmod).
%%
%% @todo The fun should be able to return a tuple (or maybe an improper
%% list, like a generator?) with the nodename and a new fun to replace
%% itself.
append_to_dispatcher_list(Mod, Item) ->
   gen_server:call(?SERVER, {append_to_dispatcher_list, Mod, Item}).

%% @spec dispatcher_list(Module::atom(), Dispatcher) ->
%%    ok
%%    Dispatcher = Node + function()
%%    Node = atom()
%% @doc Removes the entry from the dispatcher list for the given
%% subapp. Note: this always succeeds, even if the item was not
%% in the list. For funs to compare equal, they need to be defined
%% in the same context and consist of the same forms.
remove_from_dispatcher_list(Mod, Item) ->
   gen_server:call(?SERVER, {remove_from_dispatcher_list, Mod, Item}).

info() ->
   gen_server:call(?SERVER, info).

info(Mod) ->
   gen_server:call(?SERVER, {info, Mod}).

init([]) ->
   %% TODO: configure
   %% spewf is the hardcoded default--all local sessions
   {ok, dict:store(spewf, #sessionspec{next = [local], done = []}, dict:new())}.

handle_call(info, _From, State) ->
   {reply, State, State};
handle_call({info, Mod}, _From, State) ->
   {reply, dict:find(Mod, State), State};
handle_call({remove_from_dispatcher_list, Mod, Item}, _From, State) ->
   {reply, ok, handle_remove(Mod, Item, State)};
handle_call({append_to_dispatcher_list, Mod, Item}, _From, State) ->
   {reply, ok, handle_append(Mod, Item, State)};
handle_call({unregister_subapp, spewf}, _From, State)->
   {reply, {error, cannot_unregister_default}, State};
handle_call({unregister_subapp, Mod}, _From, State) ->
   {reply, ok, dict:erase(Mod, State)};
handle_call({register_subapp, spewf, []}, _From, State) ->
   {reply, {error, {cannot_register_empty_default, spewf}}, State};
handle_call({register_subapp, Mod, Dlist}, _From, State) ->
   {reply, {registered, Mod},
          dict:store(Mod, #sessionspec{next = Dlist}, State)};
handle_call({next, Mod}, _From, State) ->
   {Next, Newstate, Keymod, Moddlist} = get_next(Mod, State),
   Reply = if
              is_function(Next) -> 
                 Info = erlang:fun_info(Next),
                 case lists:keysearch(arity, 1, Info) of
                    {value, {arity, 0}} -> Next();
                    {value, {arity, 1}} -> Next(Moddlist#sessionspec.last);
                    _ ->
                       {error, incorrect_arity_in_fun}
                 end;
              true -> Next
           end,
   {reply, Reply,
    dict:store(Keymod, Moddlist#sessionspec{last = Reply}, Newstate)};
handle_call(Request, From, State) ->
   {reply, {what_is_that, Request, From, State}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

get_next(Mod, D) ->
   {RMod, Spec} = case dict:find(Mod, D) of
                     {ok, Rec} -> {Mod, Rec};
                     error -> {spewf, dict:fetch(spewf, D)}
                  end,
   {Next, NewSpec} = get_next_for(Spec),
   {Next, dict:store(RMod, NewSpec, D), RMod, NewSpec}.

get_next_for(#sessionspec{next = [], done = Done, last = Last}) ->
   get_next_for(#sessionspec{next = lists:reverse(Done), done = [],
                             last = Last});
get_next_for(#sessionspec{next = [Next|Rest], done = Done, last = Last}) ->
   {Next, #sessionspec{next = Rest, done = [Next|Done], last = Last}}.

handle_append(Mod, Item, D) ->
   case dict:find(Mod, D) of
      {ok, #sessionspec{next = Next, done = Done, last = Last}} ->
         dict:store(Mod, #sessionspec{next = Next ++ [Item], done = Done,
                                      last = Last}, D);
      _ ->
         dict:store(Mod, #sessionspec{next = [Item], done = [],
                                      last = undefined}, D)
   end.

handle_remove(spewf, Item, D) ->
   case dict:find(spewf, D) of
      {ok, #sessionspec{next = [Item], done = []}} -> D;
      {ok, #sessionspec{next = [], done = [Item]}} -> D;
      {ok, Spec} ->
         do_remove(spewf, Item, D, Spec)
   end;
handle_remove(Mod, Item, D) ->
   case dict:find(Mod, D) of
      {ok, Spec} -> do_remove(Mod, Item, D, Spec);
      _ -> D
   end.

do_remove(Mod, Item, D, #sessionspec{next = Next, done = Done, last = Last}) ->
   IsItem = fun(X) -> X == Item end,
   New = 
      case lists:any(IsItem, Next) of
         true ->
            #sessionspec{next = lists:delete(Item, Next),
                         done = Done, last = Last};
         _ ->
            case lists:any(IsItem, Done) of
               true ->
                  #sessionspec{next = Next,
                               done = lists:delete(Item, Done),
                               last = Last};
               _ ->
                  #sessionspec{next = Next, done = Done, last = Last}
            end
      end,
   dict:store(Mod, New, D).

do_remove_test_() ->
   Setup = fun (Rm, Next, Done) ->
                 dict:fetch(webecho,
                            do_remove(webecho, Rm,
                                      dict:store(webecho,
                                                 #sessionspec{next = Next,
                                                              done = Done,
                                                              last = l1},
                                                 dict:new()),
                                     #sessionspec{next = Next,
                                                  done = Done,
                                                  last = l1}))
           end,
   [
    ?_assertMatch(#sessionspec{next = [two], done = [], last = l1},
                  Setup(one, [one, two], [])),
    ?_assertMatch(#sessionspec{next = [], done = [two], last = l1},
                  Setup(one, [one], [two])),
    ?_assertMatch(#sessionspec{next = [two], done = [], last = l1},
                  Setup(one, [two], [one])),
    ?_assertMatch(#sessionspec{next = [], done = [two], last = l1},
                  Setup(one, [], [one, two]))
   ].
                                                               


%% Tests
basic_test_() ->
   [
    ?_assertMatch({ok, _Pid}, start_link()),
    ?_assertMatch(local, next(webecho)),
    ?_assertMatch(ok, stop()),
    ?_assertMatch(ok, receive after 1000 -> ok end),
    ?_assertMatch(undefined, whereis(spewf_server))
   ].

dynamic_test_() ->
   F = fun (thisnode) ->
             thatnode;
           (thatnode) ->
             thisnode;
           (_) ->
             thisnode
       end,
   [
    ?_assertMatch({ok, _Pid}, start_link()),
    ?_assertMatch({registered, webecho},
                  register_subapp(webecho, [F])),
    ?_assertMatch(thisnode, next(webecho)),
    ?_assertMatch(thatnode, next(webecho)),
    ?_assertMatch(thisnode, next(webecho)),
    ?_assertMatch(thatnode, next(webecho)),
    ?_assertMatch(ok, append_to_dispatcher_list(webecho,
                                                newnode)),
    ?_assertMatch(newnode, next(webecho)),
    ?_assertMatch(thisnode, next(webecho)),
    ?_assertMatch(newnode, next(webecho)),
    ?_assertMatch(thisnode, next(webecho)),
    ?_assertMatch(ok, remove_from_dispatcher_list(webecho, F)),
    ?_assertMatch(newnode, next(webecho)),
    ?_assertMatch(newnode, next(webecho)),
    ?_assertMatch(ok, unregister_subapp(webecho)),
    ?_assertMatch(local, next(webecho)),
    ?_assertMatch(ok, stop()),
    ?_assertMatch(ok, receive after 1000 -> ok end),
    ?_assertMatch(undefined, whereis(spewf_server))
   ].
