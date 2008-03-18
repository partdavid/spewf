%% @author partdavid@gmail.com
%% @doc Test module for spewf; mainly echoes the request and provides
%% a field for form input. This is essentially an implementation of
%% the [http://www.paulgraham.com/arcchallenge.html arc challenge] plus
%% additional information (including all the fields in the request).
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
-module(webecho).

-behaviour(spewf_session).

-include_lib("eunit/include/eunit.hrl").

-export([init/1,
         handle_request/2]).

-record(state, {said = ""}).

init([]) ->
   {ok, #state{said="You said nothing yet"}}.

%% @spec handle_request(Request, State) -> {Reply, NewState}
%%    State = #state{}
%%    NewState = #state{}
%%    Request = request()
%% @doc The spewf_session callback for handling the reply.
handle_request(R, S) ->
   Said = case lists:keysearch(said, 1, R) of
             {value, {said, Value}} -> Value;
             false -> "You said nothing"
          end,
   Reply = {spewf, [], [{h2, [], "WEBECHO Application"},
                    {p, [], ["Before: ", S#state.said]},
                    {p, [], ["This time: ", Said]},
                    {form, [{method, post}],
                     [{input, [{type, "text"}, {name, "said"}, {size, 50}], []},
                      {input, [{type, "submit"}], []}]},
                    show_req(R)
                   ]},
   {Reply, #state{said = Said}}.

show_req(R) ->
   show_req(R, []).

show_req([], A) ->
   {table, [{border, none}], lists:reverse(A)};
show_req([{K, V}|R], A) ->
   Ks = atom_to_list(K),
   show_req(R, [{tr, [],
                 [
                  {td, [{align, "left"}], [{b, [], Ks}, ":"]},
                  {td, [{align, "left"}], io_lib:format("~p", [V])}
                 ]}|A]).
