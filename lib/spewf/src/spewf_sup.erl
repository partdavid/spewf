%% @doc This is the spewf master supervisor. It supervises the session
%% manager tree (which determines at what dispatcher to start new sessions)
%% and the dispatcher tree (which dispatches every request to the
%% appropriate session).
-module(spewf_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/1,
         start_shell/1
        ]).

-export([
         init/1
        ]).

-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the spewf supervisor.
%% @spec start_link(StartArgs) -> {ok, pid()} | Error
%% @end
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Unlink the pid so our bad shell expressions don't kill the
%% tree.
start_shell(Args) ->
   {ok, Pid} = start_link(Args),
   unlink(Pid),
   {ok, Pid}.

%% Starts the two supervisor subtrees, the dispatcher subtree and
%% the session manager (spewf_server) subtree
init([]) ->
   RestartStrategy    = one_for_one,
   MaxRestarts        = 1000,
   MaxTimeBetRestarts = 3600,
   
   SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
   
   ChildSpecs =
      [
       {spewf_server_sup,
        {spewf_server_sup, start_link, [none]},
        permanent,
        1000,
        supervisor,
        [spewf_server_sup]},
       {spewf_dispatch_sup,
        {spewf_dispatch_sup, start_link, [none]},
        permanent,
        1000,
        supervisor,
        [spewf_dispatch_sup]}
      ],
   {ok,{SupFlags, ChildSpecs}}.
