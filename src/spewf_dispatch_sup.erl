%% @doc This is the spewf dispatcher supervisor. It supervises this local
%% dispatcher as well as the sessions the dispatcher creates.
-module(spewf_dispatch_sup).

-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").

-export([
         start_link/1,
         start_shell/1
        ]).

-export([
         init/1,
         start_session/1,
         start_session/2,
         start_session/4
        ]).

-define(SERVER, ?MODULE).

%% @doc Starts the spewf supervisor.
%% @spec start_link(StartArgs) -> {ok, pid()} | Error
%% @end
start_link(_StartArgs) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_shell(Args) ->
   {ok, Pid} = start_link(Args),
   unlink(Pid),
   {ok, Pid}.

start_session(Id, SessionArgs) ->
   supervisor:start_child(?SERVER, {Id,
                                    {spewf_session, start_link, SessionArgs},
                                    transient,
                                    1000,
                                    worker,
                                    [spewf_session]}).

start_session([Mod, From, Sid, InitRequest]) ->
   start_session(Sid, [Mod, From, Sid, InitRequest]).

start_session(Mod, From, Sid, InitRequest) ->
   start_session(Sid, [Mod, From, Sid, InitRequest]).

init([]) ->
   RestartStrategy    = one_for_one,
   MaxRestarts        = 1000,
   MaxTimeBetRestarts = 3600,
   
   SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
   
   ChildSpecs =
      [
       {spewf_dispatcher,
        {spewf_dispatcher, start_link, []},
        permanent,
        1000,
        worker,
        [spewf_dispatcher]}
      ],
   {ok,{SupFlags, ChildSpecs}}.

%% These require supervisor to have been started
basic_test_() ->
   [
    ?_assert(lists:member(spewf_dispatcher, registered()))
   ].
