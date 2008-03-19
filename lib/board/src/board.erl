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
-include_lib("yaws/include/yaws_api.hrl").

-include("board.hrl").

-export([init/1,
         handle_request/2,
         yaws_out/1]).

%% ----------------------------------------------
%% spewf_session callbacks
%% These functions implement stateful session
%% logic using SPEWF callbacks
%% ----------------------------------------------
-record(state, {user, post, said}).

page(Title, Content, Req, State) ->
   [{head, [],
     [{link, [{rel, "stylesheet"},
              {type, "text/css"},
              {href, "/board.css"}], []},
      {title, Title}]},
    {body, [],
     [{h2, [{class, title}], Title},
      {'div', [{id, menu}],
       [{p, [{class, mtitle}], "Actions"},
        {p, [{class, mitem}], dim({self, [{action, "new"}], "New"}, Req)},
        {p, [{class, mitem}], dim({self, [{action, "threads"}],
                                   "Threads"}, Req)},
        {p, [{class, mitem}], dim({self, [{action, "start"}], "Start"}, Req)},
        {p, [{class, mitem}], loglink(State#state.user)}]},
      {'div', [{id, main}], Content}]}].

loglink(undefined) ->
   {self, [{action, "login"}], "Login"};
loglink(#user{nickname = undefined}) ->
   {self, [{action, "login"}], "Login"};
loglink(#user{nickname = Nick}) ->
   {self, [{action, "logout"}], ["Logout "|Nick]}.

dim(El = {self, Attr, Text}, Req) ->
   case lists:keysearch(action, 1, Attr) of
      Act ->
         case lists:keysearch(action, 1, Req) of
            Act -> Text;
            _ -> El
         end;
      _ -> El
   end.
   

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
   Reply = {spewf,
            [], page("BOARD Application",
                     [{p, [], ["Before: ", S#state.said]},
                      {p, [], ["This time: ", Said]},
                      {form, [{method, post}],
                       [{input, [{type, "text"}, {name, "said"}, {size, 50}], []},
                        {input, [{type, "submit"}], []}]},
                      show_req(R)], R, S)},
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


yaws_out(A) ->
   R = spewf:make_req(A),
   spewf_lang:trans({spewf,
                     [{subapp, ?MODULE}],
                     page("BOARD Application",
                          [{p, [], "Stateless/dynamic content"}], R, #state{})}).
                      
   
