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
-record(state, {user, draft, said}).

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
        {p, [{class, mitem}], dim({self, [{action, "post"},
                                          {parent, "root"}], "Start"}, Req)},
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
   case spewf:val(action, R) of
      "post" ->
         do_post(R, S);
      "logout" ->
         do_logout(R, S);
      "profile" ->
         do_profile(R, S);
      "show" ->
         do_show(R, S);
      "threads" ->
         do_threads(R, S);
      "authenticate" ->
         do_authenticate(R, S);
      "login" ->
         do_login(R, S);
      _ ->
         do_new(R, S)
   end.

wrap(Reply, R, S) when not is_list(Reply) ->
   wrap([Reply], R, S);
wrap(Reply, R, S) ->
   {{spewf, [], page("BOARD Application",
                     Reply ++ [show_req(R, S)], R, S)},
    S}.

do_logout(R, S) ->
   handle_request(spewf:set_val(action, "new", R),
                  S#state{user = undefined}).

do_authenticate(R, S) ->
   Username = spewf:val(username, R),
   Password = spewf:val(password, R),
   case bd_db:authenticate(Username, Password) of
      {ok, Uid} ->
         User = bd_db:find_user(#user{id = Uid}),
         handle_request(spewf:set_val(action, "new", R),
                        S#state{user = User});
      Err ->
         wrap(["Invalid username or password",
               login_form(R)], R, S)
   end.

do_show(R, S) ->
   wrap(
     [{h3, [], "Post"}] ++
     case spewf:val(post, R) of
        undefined ->
           [{self, [{action, new}], "No post specified."}];
        PId ->
           Thread = bd_db:thread(#post{id = bd_db:s2id(PId)}),
            bd_web:show_thread(Thread, S#state.user)
     end, R, S).

do_profile(R, S) ->
   case spewf:val(user, R) of
      undefined ->
         handle_request(spewf:set_val(action, "new", R), S);
      Uid ->
         case bd_db:find_user(bd_db:s2id(Uid)) of
            #user{nickname = Nick,
                  text = Text} ->
               wrap([{h2, [], Nick},
                     Text], R, S);
            Err ->
               wrap([{h2, [], "Error"},
                     "No such user."], R, S)
         end
   end.

do_threads(R, S = #state{user = undefined}) ->
   do_login(R, S);
do_threads(R, S = #state{user = User}) ->
   Posts = bd_db:find_posts(#post{author = User#user.id}),
   wrap(
     [{h2, [], "Threads"}] ++
     lists:map(fun (Post) ->
                     Thread = bd_db:thread(Post),
                     bd_web:show_thread(Thread, User)
               end, Posts),
     R, S).

do_post(R, S = #state{user = undefined}) ->
   handle_request(spewf:set_val(action, "login", R), S);
do_post(R, S) ->
   case spewf:val(preview, R) of
      "preview" ->
         Draft = #post{summary = spewf:val(summary, R),
                       text = spewf:val(text, R),
                       parent = make_parent(spewf:val(parent, R))},
         wrap([{h3, [], "Preview"}]
              ++ bd_web:show_post_form(
                   spewf:val(parent, R), Draft, S#state.user),
               R, S#state{draft = Draft});
      _ ->
         case spewf:val(save, R) of
            "save" ->
               Author = S#state.user,
               Post = #post{summary = spewf:val(summary, R),
                            author = Author#user.id,
                            text = make_binary(spewf:val(text, R)),
                            parent = make_parent(spewf:val(parent, R))},
               case bd_db:new_post(Post) of
                  {ok, PId} ->
                     NewPost = bd_db:find_post(#post{id = PId}),
                     wrap([{h3, [], "Saved"}]
                          ++ [bd_web:show_post(0, NewPost, S#state.user)],
                          R, S#state{draft = undefined});
                  Error ->
                     wrap([{h3, [], "Error saving post"},
                           {pre, [{class, "error"}],
                            io_lib:format("~p", [Error])}],
                          R, S#state{draft = undefined})
               end;
            _ ->
               wrap([{h3, [], "New post"},
                     bd_web:show_post_form(
                       spewf:val(parent, R), undefined, S#state.user)],
                    R, S)
         end
   end.
                     
                          

make_binary(L) when is_list(L) ->
   list_to_binary(L);
make_binary(B) when is_binary(B) ->
   B.

make_parent("root") ->
   root;
make_parent(P) when is_list(P) ->
   bd_db:s2id(P);
make_parent(P) -> P.

do_new(R, S) ->
   wrap([{h3, [], "New Posts"}] ++
        lists:map(fun bd_web:show_summary/1,
                  bd_db:latest_posts(10)), R, S).

do_login(R, S) ->
   wrap([{h3, [], "Login"},
         login_form(R)], R, S).

login_form(R) ->
   {form, [{method, post},
           {action, "/spewf/board"}],
    [
     {input, [{type, "hidden"},
              {name, "debug"},
              {value, "true"}], []},
     {input, [{type, "hidden"},
              {name, "action"},
              {value, "authenticate"}], []},
     {table, [{class, "list"}],
      {tbody,
       [{tr,
         [{th, "Username:"},
          {td, {input, [{type, "text"}, {name, "username"}], []}}]},
        {tr,
         [{th, "Password:"},
          {td, {input, [{type, "password"}, {name, "password"}], []}}]},
        {tr,
         [{th, "&nbsp;"},
          {td, {input, [{type, "submit"}, {name, "submit"},
                        {value, "submit"}], []}}]}
        ]}}
     ]}.
         

show_req(R, S) ->
   show_req(R, S, []).

show_req([], S, A) ->
   {table, [{class, "little list"}, {style, "color: gray"}], lists:reverse(A)
   ++ [show_state(S)]};
show_req([{K, V}|R], S, A) ->
   Ks = atom_to_list(K),
   show_req(R, S, [{tr, [],
                 [
                  {td, [{align, "left"}], [{b, [], Ks}, ":"]},
                  {td, [{align, "left"}], io_lib:format("~p", [V])}
                 ]}|A]).

show_state(State) ->
   {tr, [], [
             {td, [{align, "left"}], [{b, [], "State:"}]},
             {td, [{align, "left"}], io_lib:format("~p", [State])}
             ]}.


yaws_out(A) ->
   R = spewf:make_req(A),
   spewf_lang:trans({spewf,
                     [{subapp, ?MODULE}],
                     page("BOARD Application",
                          [{p, [], "Stateless/dynamic content"}], R, #state{})}).
                      
   
