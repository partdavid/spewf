%% @author partdavid@gmail.com
%% @doc XSLT? No, spewf_lang. This module translates the
%% ``{spewf, Opt, Content}'' from a SPEWF application into
%% HTML (well, ``ehtml'' as expected by yaws). The spewf language
%% accommodates user-defined callbacks and template translators.
%%
%% The things in a spewf tree are always three-tuples. The first
%% element is an atom describing what it is. The second is a keylist
%% of attributes, and the third is a spewf thing, a list of spewf things,
%% or an iolist.
%%
%% @type spewfthing() = {atom(), optlist(), spewfcontent()}
%% @type optlist() = [{atom(), string() + atom()}]
%% @type spewfcontent() = spewfthing() + [spewfthing()] + iolist()
%%
%% Normalizations:
%%
%% All elements are normalized as three-tuples before calling any
%% callback functions.
%%
%% The following transformations are carried out:
%%
%% <ul>
%%    <li>Option values are turned from atoms to strings. Option
%%        specifiers are assured t</li>
%%    <li>The top-level thing is turned into an ``ethml'' two-tuple.</li>
%%    <li>A reasonable xhtml declaration is provided.</li>
%%    <li>A whole page is generated.</li>
%%    <li>Hidden fields with the SPEWF session Id are added to forms.</li>
%%    <li>If the attribute value of any element is 'self', it is replaced
%%        With a self-referencing URL including the sessiod in</li>
%%    <li>The element 'self' is replaced with an a element, the href
%%        is set to a self-referencing URL including the SPEWF session
%%        Id in the query string; additionally, the query string will
%%        contain any attributes of the 'self' element that are not
%%        standard attributes of the 'a' element.</li>
%%    <li>Any naked atoms as element content get turned into
%%        lists</li>
%% </ul>
%%
%% @todo Allow {partial, true} to prevent the wrapping in HTML body.
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
-module(spewf_lang).

-export([trans/1, do_surj/2, surj/0, surj/1]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(default_header, ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                         "<!DOCTYPE html PUBLIC \"-//W3C//DTD "
                         "XHTML 1.0 Strict//EN\" \"DTD/xhtml-strict1.dtd\">")).

-define(is_attr_a(X), (X == name orelse
                       X == href orelse
                       X == title orelse
                       X == target orelse
                       X == rel orelse
                       X == rev)).

surj() ->
   D = [],
   make_surj(D).

surj(D) when is_list(D) ->
   make_surj(D).

make_surj(D) ->
   fun(A) ->
         ?MODULE:do_surj(A, D)
   end.

do_surj({each}, D) ->
   D;
do_surj({keys}, D) ->
   [ K || {K, V} <- D ];
do_surj({exists, K}, D) ->
   lists:keymember(K, 1, D);
do_surj({K, V}, D) ->
   surj(lists:keystore(K, 1, D, {K, V}));
do_surj({unset, K}, D) ->
   surj(lists:keydelete(K, 1, D));
do_surj(K, D) ->
   case lists:keysearch(K, 1, D) of
      {value, {K, V}} -> V;
      _ -> undefined
   end.

surj_unset(K, S) ->
   S({unset, K}).

surj_keys(S) ->
   S({keys}).

surj_exists(K, S) ->
   S({exists, K}).

surj_each(S) ->
   S({each}).

surj_or(K, S, Default) ->
   case S(K) of
      undefined -> Default;
      Val -> Val
   end.


trans({spewf, InTree}) ->
   trans({spewf, [], InTree});
trans({spewf, Opts, InTree}) ->
   Content = normalize(InTree),
   Opt = surj(Opts),
   case Opt(translate) of
      undefined -> {ehtml, [header(Opt), top_trans(Content, Opt)]};
      Funs ->
         {ehtml, [header(Opt), top_trans(lists:foldl(fun (F, Tree) ->
                                         F(Tree)
                                   end, Content, Funs), Opt)]}
   end;
trans(InTree) ->
   trans({spewf, [], InTree}).

header(Opt) ->
   surj_or(header, Opt, ?default_header).

top_trans({html, Content}, Opt) ->
   top_trans({html, [], Content}, Opt);
top_trans({html, HOpt, [Head, Body]}, Opt) when element(1, Head) == head,
                                                element(1, Body) == body ->
   {html, xattrs(HOpt, Opt), [trans(Head, Opt, [head]), trans(Body, Opt, [body])]};
top_trans({html, HOpt, Body}, Opt) when element(1, Body) == body ->
   top_trans({html, HOpt, [
                           {head, [],
                            {title, [],
                             surj_or(title, Opt,
                                     io_lib:format(
                                       "SPEWF application - ~s",
                                       [Opt(subapp)]))}},
                           Body]}, Opt);
top_trans({html, HOpt, Content}, Opt) ->
   top_trans({html, HOpt, {body, [], Content}}, Opt);
top_trans(Content, Opt) ->
   top_trans({html, [{xmlns, "http://www.w3.org/1999/xhtml"},
                     {'xml:lang', "en"},
                     {lang, "en"}], Content}, Opt).

in(Thing, In) ->
   lists:member(Thing, In).

hidden_field(Field, Value) ->
   {input, [{type, "hidden"},
            {name, Field},
            {value, Value}], []}.

%% self element
do_self(Attrs, Text, Opt) ->
   {AAttrs, QSparams} = lists:partition(fun ({X, _}) when ?is_attr_a(X) ->
                                               true;
                                             ({_, _}) -> false
                                         end, lists:keydelete(href, 1, Attrs)),
   URLparams =
      case Opt(spewfsid) of
         undefined -> QSparams;
         S -> [{spewfsid, S}|QSparams]
      end,
   {a,
    [{href, "?" ++ string:join([ atom_to_list(A) ++ "="
                                 ++ yaws_api:url_encode(V)
                                 || {A, V} <- URLparams ], "&"
                              )}
     | AAttrs],
    Text}.

trans({self, Attrs, Text}, Opt, In) ->
   do_self(xattrs(Attrs, Opt), Text, Opt);
trans({form, Attrs, Form}, Opt, In) when is_list(Form) ->
   {form, xattrs(Attrs, Opt), trans([hidden_field(spewfsid, Opt(spewfsid))|Form],
                               Opt, [form|In])};
trans({form, Attrs, Form}, Opt, In) ->
   trans({form, Attrs, [Form]}, Opt, In);
trans({Thing, Attrs, self}, Opt, In) ->
   {Thing, xattrs(Attrs, Opt), "?spewfsid=" ++ Opt(spewfsid)};
trans({Thing, Attrs, Content}, Opt, In) ->
   {Thing, xattrs(Attrs, Opt), trans(Content, Opt, [Thing|In])};
trans(List, Opt, In) when is_list(List) ->
   lists:map(fun (Thing) ->
                   trans(Thing, Opt, In)
             end, List);
trans(Atom, Opt, In) when is_atom(Atom) ->
   atom_to_list(Atom);
trans(Iolist, Opt, In) ->
   Iolist.

normalize({Thing, Content}) ->
   normalize({Thing, [], Content});
normalize(List) when is_list(List) ->
   lists:map(fun (Item) ->
                   normalize(Item)
             end, List);
normalize({Thing, Attrs, Content}) ->
   {Thing, Attrs, normalize(Content)};
normalize(Iolist) ->
   Iolist.


xattrs(Attrs, Opt) ->
   xattrs(Attrs, Opt, []).

xattrs([], Opt, A) ->
   lists:reverse(A);
xattrs([{K, V}|Rest], Opt, A) when is_list(K) ->
   xattrs([{list_to_atom(K), V}|Rest], Opt, A);
xattrs([{K, self}|Rest], Opt, A) ->
   xattrs(Rest, Opt, [{K, "?spewfsid=" ++ Opt(spewfsid)}|A]);
xattrs([{K, V}|Rest], Opt, A) when not is_list(V) ->
   xattrs(Rest, Opt, [{K, val_to_list(V)}|A]);
xattrs([{K, V}|Rest], Opt, A) ->
   xattrs(Rest, Opt, [{K, V}|A]).

val_to_list(T) when is_integer(T) -> integer_to_list(T);
val_to_list(T) when is_float(T) -> float_to_list(T);
val_to_list(T) when is_binary(T) ->  binary_to_list(T);
val_to_list(T) ->
   lists:flatten(io_lib:format("~w", [T])).

body(Ehtml) ->
   {ehtml, [Header, {html, _, [
                               {head, _, _},
                               {body, _, Body}
                               ]}]} = Ehtml,
   Body.
   

trans_test_() ->
   [
    ?_assertMatch({spewf, [], {h2, [], "Name"}},
                  normalize({spewf, [], {h2, "Name"}})),
    ?_assertMatch({spewf, [], [{h2, [], "Name"},
                               {p, [], "text"}]},
                  normalize({spewf, [], [{h2, "Name"},
                                         {p, "text"}]})),
    ?_assertMatch({ehtml, [_, {html, _,
                               [{head, _, {title, _, _}},
                                {body, _, "hello"}]}]},
                  trans({spewf, [], "hello"})),
    ?_assertMatch({ehtml, ["HEADER",
                           {html, _,
                            [{head, _, {title, _, "TITLE"}},
                             {body, _, {p, _, "text"}}]}]},
                  trans({spewf, [{header, "HEADER"},
                                 {title, "TITLE"}],
                         {p, "text"}})),
    ?_assertMatch({p, [{color, "red"}], "hello"},
                  body(trans({spewf, [], {p, [{"color", red}], "hello"}}))),
    ?_assertMatch({a, [{href, "?action=add"}], "Add Item"},
                  body(trans({self, [{action, add}], "Add Item"}))),
    ?_assertMatch({a, [{href, "?spewfsid=XYZ&action=add"}], "Add Item"},
                  body(trans({spewf, [{spewfsid, "XYZ"}],
                              {self, [{action, add}], "Add Item"}}))),
    ?_assertMatch({form, [],
                   [{input, [{type, "hidden"},
                            {name, "spewfsid"},
                            {value, "undefined"}], []},
                    {input, [{type, "text"}, {name, "test"}], []}
                    ]},
                  body(trans({form, [], {input,
                                         [{type, text}, {name, "test"}],
                                         []}})))

   ].
