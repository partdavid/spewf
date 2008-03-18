-module(bd_sup).

-behaviour(supervisor).

-export([
         start_link/1
        ]).

-export([
         init/1
        ]).

-define(SERVER, ?MODULE).

%% @spec start_link(StartArgs) -> {ok, pid()} + {error, Reason}
%% @doc Start the board supervisor.
start_link(_StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @spec init(Args) -> {ok, pid()} + {error, Reason}
%% @doc Initialize the supervisor.
init([]) ->
   RestartStrategy    = one_for_one,
   MaxRestarts        = 1000,
   MaxTimeBetRestarts = 3600,
   
   SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
   
   ChildSpecs =
      [
       {bd_server,
        {bd_server, start_link, []},
        permanent,
        1000,
        worker,
        [bd_server]}
      ],
    {ok,{SupFlags, ChildSpecs}}.
