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
-module(spewf_sup).
%% @doc This is the spewf master supervisor. It supervises the session
%% manager tree (which determines at what dispatcher to start new sessions)
%% and the dispatcher tree (which dispatches every request to the
%% appropriate session).

-behaviour(supervisor).

%% API
-export([
         start_link/1,
         start_shell/1
        ]).

-export([
         init/1
        ]).

-define(SERVER, ?MODULE).

%% @spec start_link(none) -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Start the master supervisor. The argument is ignored (for now).
start_link(_StartArgs) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @spec start_shell(Arg::term()) -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Start the master supervisor without linking to the current process.
%% Handy for testing so bad shell expressions don't kill the tree.
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
