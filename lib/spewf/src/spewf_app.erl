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
-module(spewf_app).
%% @author partdavid@gmail.com
%% @doc Application callback module for SPEWF.

-behaviour(application).

-export([
         start/0,
         start/2,
         shutdown/0,
         stop/1
        ]).

%% @spec start() -> {ok, pid()} + {error, Reason}
%% @doc Start the SPEWF application.
start() ->
   application:start(spewf).

start(Type, StartArgs) ->
   case application:get_env(spewf, start_yaws) of
      {ok, {DocRoot, ServerConf, GlobalConf}} ->
         case yaws:start_embedded(DocRoot, add_spewf(ServerConf), GlobalConf) of
            ok ->
               start_self(Type, StartArgs);
            Err ->
               {error, {yaws_error, Err}}
         end;
      _ ->
         %% We're not starting yaws
         start_self(Type, StartArgs)
   end.
         
add_spewf(ServerConf) ->
   case lists:keysearch(appmods, 1, ServerConf) of
      {value, {appmods, Appmods}} ->
         lists:keystore(appmods, 1, ServerConf, {appmods, [spewf|Appmods]});
      false ->
         lists:keystore(appmods, 1, ServerConf, {appmods, [spewf]})
   end.

start_self(_Type, StartArgs) ->
   case spewf_sup:start_link(StartArgs) of
      {ok, Pid} -> 
         {ok, Pid};
      Error ->
         Error
   end.

%% @spec shutdown() -> ok
%% @doc Stop the SPEWF application.
shutdown() ->
    application:stop(spewf).

%%====================================================================
%% Internal functions
%%====================================================================

stop(_State) ->
    ok.
