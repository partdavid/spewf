-module(bd_db).

%% Basic database operations
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-include("board.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(atomic(X), begin {atomic, R__} = mnesia:transaction(fun() -> X end), R__ end).

value_of(K, KL) ->
   case lists:keysearch(K, 1, KL) of
      {value, {K, V}} -> V;
      _ -> undefined
   end.

make_guid() ->
   <<N1:32/integer, N2:32/integer, N3:32/integer, N4:32/integer>> =
      crypto:rand_bytes(16),
   {N1, N2, N3, N4}.

new_user(U = #user{email = E, nickname = N}) when E /= undefined,
                                                  N /= undefined ->
   XCreate = fun() ->
                   case find_user1(U) of
                      false ->
                         NU = U#user{id = make_guid()},
                         mnesia:write(NU),
                         NU;
                      #user{email = E} -> {error, email_in_use};
                      #user{nickname = N} -> {error, nickname_in_use}
                   end
             end,
   ?atomic(XCreate).

get_post(I) ->
   txq1(qlc:q([P || P <- mnesia:table(post),
                    P#post.id =:= I])).

txq1(Q) ->
   {atomic, Result} = mnesia:transaction(fun() -> q1(Q) end),
   Result.

q1(Q) ->
   case qlc:e(Q) of
      [R|_] ->
         R;
      [] -> false
   end.

txqall(Q) ->
   {atomic, Result} = mnesia:transaction(fun() -> qlc:e(Q) end),
   Result.

niceq(Q) ->
   lists:map(fun nice/1, txqall(Q)).

find_users(U) ->
   ?atomic(find_users1(U)).

find_users1(#user{email = E, nickname = N}) when E /= undefined,
                                                N /= undefined ->
   Q = qlc:q([ U || U <- mnesia:table(user),
                    U#user.email =:= E orelse U#user.nickname =:= N]),
   qlc:e(Q);
find_users1(Id = {N1, _, _, _}) when is_integer(N1) ->
   find_users1(#user{id = Id});
find_users1(S) when not is_record(S, user) ->
   find_users1(#user{email = S, nickname = S});
find_users1(U) ->
   mnesia:match_object(set_defaults(U, #user{_ = '_'})).

find_user(U) ->
   ?atomic(find_user1(U)).

find_user1(U) ->
   case find_users1(U) of
      [User|_] ->
         User;
      _ ->
         false
   end.

new_post1(P) ->
   mnesia:write(normalize_post1(P#post{id = make_guid(),
                                      timestamp = calendar:universal_time()})).

normalize_user(U) ->
   ?atomic(normalize_user1(U)).

id2s(Id = {N, _, _, _}) when is_integer(N) ->
   urlsafe(base64:encode_to_string(term_to_binary(Id)));
id2s(X) -> X.

s2id(Id) when is_list(Id) ->
   binary_to_term(base64:decode(urlunsafe(Id))).

normalize_user1(Id = {N, _, _, _}) when is_integer(N) ->
   Id;
normalize_user1(EorN) when is_list(EorN) ->
   case find_user1(#user{nickname = EorN, email = EorN}) of
      #user{id = Id} ->
         Id;
      _ -> undefined
   end;
normalize_user1(X) -> X.

urlunsafe(L) when length(L) rem 4 > 0 ->
   urlunsafe(L ++ "=");
urlunsafe(L) ->
   lists:map(fun ($-) -> $+;
                 ($_) -> $/;
                 (C) -> C
             end, L).
                             
urlsafe(L) ->
   lists:map(fun ($+) -> $-;
                 ($/) -> $_;
                 (C) -> C
             end, string:strip(L, right, $=)).

nice(P = #post{id = Id, author = Uid, parent = Parid}) ->
   User = find_user(Uid),
   P#post{id = id2s(Id),
          parent = id2s(Parid),
          author = User#user.nickname};
nice(U = #user{id = Uid, friends = Friends, watching = Watch}) ->
   NiceFriends = lists:map(fun (F) ->
                                 User = find_user(F),
                                 User#user.nickname
                           end, Friends),
   U#user{id = id2s(Uid),
          friends = NiceFriends}.

normalize_post1(P = #post{author = A}) when is_list(A) ->
   normalize_post1(P#post{author = normalize_user1(A)});
normalize_post1(P = #post{parent = Parid}) when is_list(Parid) ->
   normalize_post1(P#post{parent = s2id(Parid)});
normalize_post1(P = #post{id = Id}) when is_list(Id) ->
   normalize_post1(P#post{id = s2id(Id)});
normalize_post1(P = #post{}) ->
   P.

new_post(P) ->
   ?atomic(new_post1(P)).

update_post(P) ->
   ?atomic(update_post1(P)).

update_post1(P) ->
   case find_post1(#post{id = P#post.id}) of
      false ->
         {error, {does_not_exist, P#post.id}};
      Old ->
         New = set_defaults(P, Old),
         mnesia:write(New)
   end.
      
rawposts() ->
   mnesia:dirty_match_object(#post{_ = '_'}).

rawusers() ->
   mnesia:dirty_match_object(#user{_ = '_'}).

posts() ->
   niceq(qlc:q([ P || P <- mnesia:table(post) ])).
                            
users() ->
   niceq(qlc:q([ U || U <- mnesia:table(user) ])).

set_defaults(Rec, Def) when element(1, Rec) =:= element(1, Def),
                            is_atom(element(1, Rec)),
                            size(Rec) == size(Def) ->
   set_defaults(Rec, Def, size(Rec)).

set_defaults(Rec, _, 0) ->
   Rec;
set_defaults(Rec, Def, N) ->
   case element(N, Rec) of
      undefined ->
         set_defaults(setelement(N, Rec, element(N, Def)), Def, N - 1);
      _ ->
         set_defaults(Rec, Def, N - 1)
   end.

fix_friends() ->
   Normalize = fun (FU) ->
                     case find_user1(FU) of
                        false -> undefined;
                        #user{id = Id} ->
                           Id
                     end
               end,
   Invalid = fun (undefined) -> true;
                 (_) -> false
             end,
   Tx = fun () ->
              mnesia:foldl(
                fun (U = #user{friends = Friends}, N) ->
                      Normal =
                         lists:dropwhile(Invalid,
                                         lists:map(Normalize, Friends)),
                      case Normal of
                         Friends -> N;
                         _ ->
                            ok = mnesia:write(U#user{friends = Normal}),
                            N + 1
                      end
                end, 0, user)
        end,
   mnesia:transaction(Tx).

find_post(P) ->
   ?atomic(find_post1(P)).

find_posts1(Id = {N, _, _, _}) when is_integer(N) ->
   find_posts1(#post{id = Id});
find_posts1(#post{author = Author}) when is_list(Author) ->
   U = normalize_user1(Author),
   find_posts1(#post{author = U});
find_posts1(P = #post{id = Sid}) when is_list(Sid) ->
   find_posts1(#post{id = s2id(Sid)});
find_posts1(P) ->
   Match = set_defaults(P, #post{_ = '_'}),
   mnesia:match_object(Match).

find_post1(P) ->
   case find_posts1(P) of
      [R|_] -> R;
      [] -> false
   end.

find_posts(P) ->
   ?atomic(find_posts1(P)).

chop(L) ->
   lists:sublist(L, length(L) - 1).

build_latest_list(Post = #post{timestamp = Timestamp, id = Id},
                  {undefined, _, Max, _, _}) ->
   RSet = [{Timestamp, Id}],
   {Timestamp,
    1,
    Max,
    RSet,
    [Post]};
build_latest_list(Post = #post{timestamp = Timestamp, id = Id},
                  {Earliest, Count, Max, RSet, Results}) when Count >= Max,
                                                         Timestamp > Earliest ->
   [{_, Rm}|ShortRset] = RSet,
   NewRSet = ordsets:add_element({Timestamp, Id}, ShortRset),
   NewEarliest = element(1, hd(NewRSet)),
   {NewEarliest,
    Count,
    Max,
    NewRSet,
    [Post|lists:keydelete(Rm, 2, Results)]};
build_latest_list(Post, A = {Earliest, Count, Max, RSet, Results})
  when Count >= Max ->
   {Earliest,
    Count,
    Max,
    RSet,
    Results};
build_latest_list(Post = #post{timestamp = Timestamp, id = Id},
                  {Earliest, Count, Max, RSet, Results}) ->
   NewRSet = ordsets:add_element({Timestamp, Id}, RSet),
   NewEarliest = element(1, hd(NewRSet)),
   {NewEarliest,
    Count + 1,
    Max,
    NewRSet,
    [Post|Results]}.

order_latest_posts({_, _, _, RSet, Results}) ->
   lists:map(fun ({_, Id}) ->
                   case lists:keysearch(Id, 2, Results) of
                      {value, Post} ->
                         Post;
                      _ ->
                         internal_inconsistency
                   end
             end, lists:reverse(RSet)).
                
latest_posts(Max) ->
   Tx = fun() ->
         mnesia:foldl(fun (P, Acc) ->
                            build_latest_list(P, Acc)
                      end,
                      {undefined, 0, Max, [], []},
                      post)
   end,
   {atomic, Results} = mnesia:transaction(Tx),
   order_latest_posts(Results).

thread(P) ->
   ?atomic(thread1(P)).

thread1(Id = {N, _, _, _}) when is_integer(N) ->
   thread1(#post{id = Id});
thread1(Pq = #post{id = Id}) ->
   P = find_post1(Pq),
   {P, lists:map(fun thread/1, find_posts1(#post{parent = Id}))}.

watching(User) ->
   ?atomic(watching1(User)).

watching1(U) when not is_record(U, user) ->
   watching1(#user{id = normalize_user1(U)});
watching1(#user{id = Id}) ->
   U = find_user1(Id),
   case U#user.watching of
      undefined -> [];
      Watching -> lists:map(fun find_post1/1, Watching)
   end.

authenticate(Username, Password) ->
   case find_user(Username) of
      #user{id = Uid, password = Password} ->
         {ok, Uid};
      #user{id = Uid} ->
         {wrong_password, Uid};
      false ->
         {nonexistent_user, Username}
   end.

update_user(U) ->
   ?atomic(update_user1(U)).

update_user1(U = #user{id = Uid}) when Uid /= undefined ->
   case find_users1(#user{id = Uid}) of
      [User] ->
         NewUser = set_defaults(U, User),
         mnesia:write(NewUser);
      [] ->
         {error, no_such_user};
      L when is_list(L) -> {error, not_unique};
      Err -> {error, Err}
   end.

create_tables(local) ->
   create_tables([node()]);
create_tables(Nodes) ->
   mnesia:create_table(post, [{attributes, record_info(fields, post)},
                              {disc_copies, Nodes}]),
   mnesia:add_table_index(post, parent),
   mnesia:create_table(user, [{attributes, record_info(fields, user)},
                              {disc_copies, Nodes}]),
   mnesia:add_table_index(user, nickname),
   mnesia:add_table_index(user, email).

load_test_data(File) ->
   {ok, Data} = file:consult(File),
   {Users, Posts, Rejects} =
      lists:foldl(fun (User, {Us, Ps, Rej}) when is_record(User, user) ->
                        new_user(User),
                        {Us + 1, Ps, Rej};
                      (Post, {Us, Ps, Rej}) when is_record(Post, post) ->
                        new_post(Post),
                        {Us, Ps + 1, Rej};
                      (Reject, {Us, Ps, Rej}) ->
                        {Us, Ps, [Reject|Rej]}
                  end, {0, 0, []}, Data),
   [{users, Users}, {posts, Posts}, {rejects, lists:reverse(Rejects)}].
                     
drop_tables() ->
   lists:map(fun mnesia:delete/1, [post, user]).
