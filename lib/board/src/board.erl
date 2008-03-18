%% @author partdavid@gmail.com
%% @doc Lightweight web posting board application, to demonstrate
%% SPEWF.
%% @end
%%
%% Copyright 2008 partdavid at gmail.com
%%
-module(board).

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
   Reply = {ehtml, [{h2, [], "BOARD Application"},
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
                  {td, [{align, "left"}], io_lib:format("~p", [V])}
                 ]}|A]).
