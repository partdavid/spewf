%% @doc Test module for spewf, merely echoes
-module(webecho).
-behaviour(spewf_session).

-include_lib("eunit/include/eunit.hrl").

-export([init/1,
         handle_request/2]).

-record(state, {said = ""}).

init([]) ->
   {ok, #state{said="You said nothing yet"}}.

handle_request(R, S) ->
   Said = case lists:keysearch(said, 1, R) of
             {value, {said, Value}} -> Value;
             false -> "You said nothing"
          end,
   Reply = {ehtml, [{h2, [], "WEBECHO Application"},
                    {p, [], ["Before: ", S#state.said]},
                    {p, [], ["This time: ", Said]},
                    {form, [{method, get}],
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
                  {td, [{align, "left"}], V}
                 ]}|A]).
