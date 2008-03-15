%% @doc This is the spewf session manager supervisor. All it does is
%% supervise the gen_server (spewf_server) that determines where the
%% next dispatcher is that should be asked for a new session. -pd
-module(spewf_server_sup).

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

start_shell(Args) ->
   {ok, Pid} = start_link(Args),
   unlink(Pid),
   {ok, Pid}.
   

init([]) ->
   RestartStrategy    = one_for_one,
   MaxRestarts        = 1000,
   MaxTimeBetRestarts = 3600,
   
   SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
   
   ChildSpecs =
      [
       {spewf_server,
        {spewf_server, start_link, []},
        permanent,
        1000,
        worker,
        [spewf_server]}
      ],
   {ok,{SupFlags, ChildSpecs}}.
