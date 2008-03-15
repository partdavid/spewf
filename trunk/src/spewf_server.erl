%% @doc This server is the "session manager". It handles the round-robin
%% handing out of session specifications, which are incomplete child
%% specifications. In its simplest form, 
-module(spewf_server).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

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

%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_shell() ->
   {ok, Pid} = start_link(),
   unlink(Pid),
   {ok, Pid}.
   

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spe1 stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

next(Mod) ->
   gen_server:call(?SERVER, {next, Mod}).

register_subapp(Mod, DispatcherList) ->
   gen_server:call(?SERVER, {register_subapp, Mod, DispatcherList}).

unregister_subapp(Mod) ->
   gen_server:call(?SERVER, {unregister_subapp, Mod}).

append_to_dispatcher_list(Mod, Item) ->
   gen_server:call(?SERVER, {append_to_dispatcher_list, Mod, Item}).

remove_from_dispatcher_list(Mod, Item) ->
   gen_server:call(?SERVER, {remove_from_dispatcher_list, Mod, Item}).

info() ->
   gen_server:call(?SERVER, info).

info(Mod) ->
   gen_server:call(?SERVER, {info, Mod}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
   %% TODO: configure
   %% spewf is the hardcoded default--all local sessions
   {ok, dict:store(spewf, #sessionspec{next = [local], done = []}, dict:new())}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
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
