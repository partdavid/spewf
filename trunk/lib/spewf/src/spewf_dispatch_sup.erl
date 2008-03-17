%% @author partdavid@gmail.com
%% @doc This is the spewf dispatcher supervisor. It supervises this local
%% dispatcher as well as the sessions the dispatcher creates.
%% @end
%%
%% @type request() = [{Key::atom(), Value::term()}].
%% @type sid() = {Node::atom(), Id::reference()}.
%% @end
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
-module(spewf_dispatch_sup).

-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").

%% API
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

%% @spec start_link(StartArgs) -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Starts the spewf supervisor.
start_link(_StartArgs) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @spec start_shell(Arg::term()) -> {ok, pid()} + {error, Reason}
%%    Reason = term()
%% @doc Start the supervisor without linking.
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

%% @spec start_session(Module, From, Sid, InitialRequest) ->
%%    {ok, Pid} + {error, Reason}
%%    Module = atom()
%%    From = pid()
%%    Sid = sid()
%%    InitialRequest = request()
%% @doc Start a transient child spewf_session process to handle the
%% initial request.
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
