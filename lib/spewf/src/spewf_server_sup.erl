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
-module(spewf_server_sup).
%% @doc This is the spewf session manager supervisor. All it does is
%% supervise the gen_server (spewf_server) that determines where the
%% next dispatcher is that should be asked for a new session. -pd

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
%% @doc Start the server supervisor. The argument is ignored.
start_link(_StartArgs) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @spec start_shell(Arg::term()) -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Start the server supervisor without linking.
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
