-module(spewf).
-export([out/1]).
-compile(export_all).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").

val(L, K) ->
	 lists:keysearch(K, 1, L).

%% TODO: configure
-define(sidkey, <<13,82,40,33,229,92,212,241,191,224,194,152,177,236,14,153>>).
-define(error_page(Err), {ehtml,
                          [
                           {head, [],
                           {title, [], "Internal error"}},
                          {body, [],
                           [
                            {h2, [{color, red}], "Internal error"},
                            {p, [], "SPEWF has encountered an internal error, "
                             "your session could not be created. Please report"
                             "the following error to the site administrator."},
                            {pre, [{color, gray}, {style, "font-size: small"}],
                             Err}
                           ]}]}).

make_mod(Data) ->
	 [Mod|_] = string:tokens(Data, "/"),
	 list_to_atom(Mod).

%% The beginnings of the "spewf template language", whatever that will
%% look like, are here. For one thing, would like to be more forgiving
%% about term types.
spewf2ehtml({spewf, What}) ->
   {ehtml, What}.

hidden_formfield(Spid) ->
   {input, [{type, "hidden"}, {name, "spewfsid"},
            {value, Spid}]}.

%% TODO: form params
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
   PidB = crypto:aes_cfb_128_decrypt(?sidkey, IVec, Sid),
   binary_to_term(PidB).

pid2sid(Pid) ->
   IVec = crypto:md5(lists:reverse(lists:flatten(
                                     [ integer_to_list(N) ||
                                        N <- tuple_to_list(now()) ]))),
   EPid = crypto:aes_cfb_128_encrypt(?sidkey, IVec, term_to_binary(Pid)),
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

out(A) ->
   R = [ {list_to_atom(K), V} || {K, V} <- yaws_api:parse_query(A) ],
   %% TODO: add regular CGI type information to request, verify
   %%       POST params, reap session information from cookies
   %% TODO: add header and other instructions to spewf response style
   %% TODO: heck, add spewf response style
   Mod = make_mod(A#arg.appmoddata),
   Answer = 
      case val(R, spewfsid) of
         {value, {spewfsid, V}} ->
            Spid = sid2pid(V),
            do_alive_request(Mod, Spid, R);
         false ->
            do_start_request(Mod, R)
      end,
   case Answer of
      {answer, NewSpid, Response} ->
         add_session(A, pid2sid(NewSpid), Response);
      Error ->
         Error
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
   ?error_page(Err).

crypt_test_() ->
   Pid = self(),
   [
    ?_assertMatch(Pid, sid2pid(pid2sid(Pid)))
   ].
