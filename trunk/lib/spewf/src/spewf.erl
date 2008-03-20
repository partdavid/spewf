%% @author partdavid@gmail.com
%% @copyright 2008 partdavid at gmail.com
%% @doc This is a yaws appmod which provides the glue between yaws and
%% the spewf application. It handles yaws requests, delegating them to
%% new or existing spewf sessions as appropriate.
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
-module(spewf).
-export([out/1, start/0, make_req/1, val/2]).
-compile(export_all).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @spec start() -> ok + {error, Reason}
%%    Reason = term()
%% @doc Starts the spewf application.
start() ->
   spewf_app:start().

%% I might want to change this someday. Maybe spewf should read an subapp
%% file of record definitions and send records to the sessions.
val(K, L) ->
   case lists:keysearch(K, 1, L) of
      {value, {K, V}} -> V;
      _ -> undefined
   end.

%% TODO: configure
%% The "hardcoded" sidkey is set at build time--if you are testing, or
%% running without a proper application config file, this can be okay. Oh,
%% this is a string because the preprocessor seems to choke on binaries
%% as the value of a macro in c() or on the erlc command line. -pd
%% -define(sidkey, "0rJnw5xro3n+q7l87lDtBg==").
-define(error_page(Err), {spewf, [],
                          [
                           {head, [],
                            {title, [], "Internal error"}},
                           {body, [],
                            [
                             {h2, [{style, "color: red"}], "Internal error"},
                             {p, [], "SPEWF has encountered an internal error, "
                              "your session could not be created. Please report "
                              "the following error to the site administrator."},
                             {pre, [{style, "color: gray; font-size: small"}],
                              io_lib:format("~p", [Err])}
                            ]}]}).

make_mod("") ->
   ' ';
make_mod("/") ->
   ' ';
make_mod(undefined) ->
   undefined;
make_mod(Data) ->
	 Mod = hd(string:tokens(Data, "/")),
	 list_to_atom(Mod).

hidden_formfield(Spid) ->
   {input, [{type, "hidden"}, {name, "spewfsid"},
            {value, Spid}]}.

process_answer(A, Mod, Spid, {ehtml, Terms}) ->
   add_session(A, Spid, {ehtml, Terms});
process_answer(A, Mod, Spid, {html, Content}) ->
   {html, Content};
process_answer(A, Mod, Spid, {spewf, Options, Content}) ->
   spewf_lang:trans({spewf, [{subapp, Mod}, {spewfsid, Spid}|Options], Content});
process_answer(_A, _Mod, _Spid, Whatever) ->
   Whatever.

%% TODO: cookies
add_session(A, Spid, {ehtml, Terms}) ->
   {ehtml, add_session(A, Spid, Terms)};
add_session(A, Spid, [T|Terms]) ->
   [add_session(A, Spid, T)|add_session(A, Spid, Terms)];
add_session(A, Spid, {form, Params, Content}) when is_list(Content) ->
   {form, Params, [hidden_formfield(Spid)|Content]};
add_session(A, Spid, {form, Params, Content}) ->
   add_session(A, Spid, {form, Params, [Content]});
add_session(A, Spid, {El, Params, Content}) ->
   {El, add_session(A, Spid, Params), add_session(A, Spid, Content)};
add_session(_A, Spid, {Thing, self}) ->
   {Thing, io_lib:format("?spewfsid=~s", [Spid])};
add_session(_A, _Spid, Any) ->
	 Any.

sid2pid(Sid) when is_list(Sid) ->
   sid2pid(base64:decode(urlunsafe(Sid)));
sid2pid(<<IVec:16/binary, Sid/binary>>) ->
   PidB = crypto:aes_cfb_128_decrypt(sidkey(), IVec, Sid),
   binary_to_term(PidB).

sidkey() ->
   case application:get_env(spewf, sidkey) of
      {ok, Key} when is_binary(Key) -> Key;
      {ok, Key} when is_list(Key) -> base64:decode(Key);
      _ -> base64:decode(?sidkey)
   end.

pid2sid(Pid) ->
   IVec = crypto:md5(lists:reverse(lists:flatten(
                                     [ integer_to_list(N) ||
                                        N <- tuple_to_list(now()) ]))),
   EPid = crypto:aes_cfb_128_encrypt(sidkey(), IVec, term_to_binary(Pid)),
   urlsafe(base64:encode_to_string(<<IVec/binary, EPid/binary>>)).

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

%% TODO: There's a DoS opportunity here with the dynamic
%% atom creation.
make_req(A) ->
   Get = [ {list_to_atom(K), V} || {K, V} <- yaws_api:parse_query(A) ],
   Req = A#arg.req,
   Post = case Req#http_request.method of
             'POST' ->
                [ {list_to_atom(K), V} || {K, V} <- yaws_api:parse_post(A) ];
             _ ->
                []
          end,
   [{yaws_arg, A}, {http_request, Req} |Get] ++ Post.

%% @spec out(Request) -> {ehtml, [term()]}
%%    Request = #arg{}
%% @doc Handles the yaws request given, returning an ehtml Erlang term
%% structure.
out(A) ->
   R = make_req(A),
   %% TODO: add regular CGI type information to request, verify
   %%       POST params, reap session information from cookies
   %% TODO: add header and other instructions to spewf response style
   %% TODO: heck, add spewf response style
   Mod = make_mod(A#arg.appmoddata),
   case is_subapp(Mod) of
      true ->
         Answer = 
            case val(spewfsid, R) of
               undefined -> do_start_request(Mod, R);
               V ->
                  Spid = sid2pid(V),
                  do_alive_request(Mod, Spid, R)
            end,
         case Answer of
            {answer, NewSpid, Response} ->
               process_answer(A, Mod, pid2sid(NewSpid), Response);
            Error ->
               Error
         end;
      _ ->
         %% TODO: This should really just be a 404 Not Found
         internal_error({not_subapp, Mod})
   end.

is_subapp(Mod) ->
   case application:get_env(spewf, subapps) of
      {ok, Subapps} when is_list(Subapps) ->
         lists:member(Mod, Subapps);
      _ ->
         Mod == webecho
   end.

do_alive_request(Mod, Spid, R) ->
   case spewf_dispatcher:request(Mod, Spid, R) of
      {answer, NewSpid, Answer} ->
         {answer, NewSpid, Answer};
      {error, timeout, _Timeout, _} ->
         do_start_request(Mod, R)
   end.

do_start_request(Mod, R) ->
   %% Find the correct dispatcher
   Next = spewf_server:next(Mod),
   case spewf_dispatcher:request(Mod, {Next, new}, R) of
      {answer, NewSpid, Answer} ->
         {answer, NewSpid, Answer};
      Error ->
         internal_error({cannot_start_session, Mod, Error})
   end.

internal_error(Err) ->
   spewf_lang:trans(?error_page(Err)).

crypt_test_() ->
   Pid = self(),
   [
    ?_assertMatch(Pid, sid2pid(pid2sid(Pid)))
   ].
