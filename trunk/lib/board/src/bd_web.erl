-module(bd_web).

-compile(export_all).

-import(bd_db, [id2s/1, s2id/1]).

-define(is_guid(N), is_integer(element(1, N))).

-define(weekdayname(N), element(N, {"Mon",
                                    "Tue",
                                    "Wed",
                                    "Thu",
                                    "Fri",
                                    "Sat",
                                    "Sun"})).

-define(monthname(M), element(M, {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"})).

-include("board.hrl").

show_timestamp({{Year, MM, Day}, {Hour, Min, Sec}}) ->
   Weekday = ?weekdayname(calendar:day_of_the_week(Year, MM, Day)),
   Month = ?monthname(MM),
   io_lib:format("~s ~w ~s ~w ~w:~.2.0w:~.2.0w",
                 [Weekday, Day, Month, Year, Hour, Min, Sec]).

show_thread(Thread, ViewUser) ->
   show_thread(0, Thread, ViewUser).

show_thread(Level, {Post, Children}, ViewUser) ->
   [show_post(Level, Post, ViewUser)] ++
      lists:map(fun (Child) ->
                      show_thread(Level + 1, Child, ViewUser)
                end, Children).

show_post(Level, Post, ViewUser) ->
   User = bd_db:find_user(Post#post.author),
   {'div', [{class, "post"},
            {style, "margin-left: " ++ integer_to_list(15 * Level) ++ "px"}],
    [{strong, [], Post#post.summary},
     {br, [], []},
     {p, [{class, "byline"}], ["posted by ",
                               {self, [{action, "profile"},
                                       {user, User#user.id}],
                                User#user.nickname},
                               " at ",
                               show_timestamp(Post#post.timestamp)
                              ]},
     {p, [], Post#post.text}] ++
    maybe_reply(Post, ViewUser)}.

maybe_reply(Post, undefined) ->
   [];
maybe_reply(Post, ViewUser) ->
   [{self, [{action, post},
            {parent, id2s(Post#post.id)}], "reply"}].

excerpt(<<Subtext:18/binary, _/binary>>) ->
   Subtext;
excerpt(Text) ->
   Text.

show_summary(#post{id = Id,
                   summary = Summary,
                   author = UId,
                   timestamp = Timestamp,
                   text = Text}) ->
   Author = bd_db:find_user(#user{id = UId}),
   {'div', [{class, "summary"}],
    [{strong, [], {self, [{action, "show"}, {post, id2s(Id)}], Summary}},
     {br, [], []},
     {p, [{class, "little"}],
      [{span, [{class, "excerpt"}], [excerpt(Text), "..."]},
       {span, [{class, "byline"}],
        [" by ",
         {self, [{action, "profile"}, {user, id2s(UId)}],
          Author#user.nickname},
         " at ",
         show_timestamp(Timestamp)]}
       ]}]}.

default_summary(root, undefined) ->
   "";
default_summary(#post{summary = Summary}, undefined) ->
   "Re: " ++ Summary;
default_summary(_, #post{summary = Summary}) ->
   Summary;
default_summary(_, _) ->
   "".

default_text(undefined) ->
   [];
default_text(#post{text = Text}) ->
   Text;
default_text(_) ->
   [].
   

show_post_form(undefined, Draft, Author) ->
   show_post_form(root, Draft, Author);
show_post_form("root", Draft, Author) ->
   show_post_form(root, Draft, Author);
show_post_form(Parent, Draft, Author) when is_list(Parent) ->
   show_post_form(s2id(Parent), Draft, Author);
show_post_form(Parent, Draft, Author) when ?is_guid(Parent) ->
   show_post_form(bd_db:find_post(Parent), Draft, Author);
show_post_form(Parent, Draft, Author) ->
   [show_draft(Draft, Author),
    {form, [{action, "/spewf/board"},
            {method, "post"}],
     [{input, [{type, "hidden"},
               {name, "action"},
               {value, "post"}], []},
      {input, [{type, "text"},
               {name, "summary"},
               {size, "30"},
               {value, default_summary(Parent, Draft)}], []},
      {br, [], []},
      {textarea, [{cols, "50"},
                  {rows, "25"},
                  {name, "text"}], default_text(Draft)},
      {br, [], []},
      {input, [{type, "submit"},
               {name, "preview"},
               {value, "preview"}], []},
      {input, [{type, "submit"},
               {name, "save"},
               {value, "save"}], []}]}
   ].

show_draft(undefined, Author) ->
   [];
show_draft(Draft = #post{}, Author) ->
   show_post(0,
             Draft#post{author = Author#user.id,
                        timestamp = calendar:universal_time()},
             undefined).
                        
