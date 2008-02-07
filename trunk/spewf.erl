%% Session Process Erlang Web Framework
%%
%% Clever web frameworks have been written in other language, taking advantage
%% of those languages' idioms and features to provide session (that is,
%% server-stateful) support. There are one or two excellent web frameworks
%% for Erlang that (at my cursory and ignorant glance) follow the idioms
%% of existing web frameworks more than the idioms of Erlang.
%%
%% The idioms (that will be) reflected here are twofold: "everything" is a
%% process; namely, all sessions' states are stored in individual process.
%% Secondly, callbacks (reflecting implementation of a behavior) are used
%% by the web application writer to provide their application logic.
%%
%% See the README.txt file for information on running and using SPEWF.
%%
%% Copyright (c) 2008 partdavid@gmail.com
%%
%% SPEWF, the Session Process Web Framework, is available under the terms of
%% the Lesser GNU General Public License (LGPL). You should have received
%% a copy of this license with the program. See the file COPYING.
%%

-module(spewf).
-author('partdavid@gmail.com').
-vsn({0, 1, 1}).
-export([val/2, out/1, handle/2]).

-include_lib("yaws_api.hrl").

val(L, K) ->
	 lists:keysearch(K, 1, L).

new_session(Mod) ->
	 spawn(?MODULE, handle, [Mod, []]).

handle(M, S) ->
	 receive
		  {From, {request, Req}} ->
				{NewS, Reply} = apply(M, handle, [S, Req]),
				From ! {self(), Reply},
				?MODULE:handle(M, NewS);
		  {_From, 'EXIT'} -> exit
	 after 900000 ->
				timeout
	 end.

make_mod(Data) ->
	 [Mod|_] = string:tokens(Data, "/"),
	 list_to_atom(Mod).

%% TODO This is quite limited at the moment, only 'self' is supported,
%% but one needs various ways of including or transforming that part of
%% the URL. Also, there should be some way of indicating that you want
%% the session in a hidden field or something similar. -partdavid
add_session(A, Spid, {ehtml, Terms}) ->
	 {ehtml, add_session(A, Spid, Terms)};
add_session(A, Spid, [T|Terms]) ->
	 [add_session(A, Spid, T)|add_session(A, Spid, Terms)];
add_session(A, Spid, {El, Params, Content}) ->
	 {El, add_session(A, Spid, Params), add_session(A, Spid, Content)};
add_session(_A, Spid, {Thing, self}) ->
	 {Thing, io_lib:format("?spid=~s", [pid_to_list(Spid)])};
add_session(_A, _Spid, Any) ->
	 Any.

out(A) ->
	 R = [ {list_to_atom(K), V} || {K, V} <- yaws_api:parse_query(A) ],
	 Spid = case val(R, spid) of
					{value, {spid, V}} ->
						 Pid = list_to_pid(V),
						 case is_process_alive(Pid) of
							  true -> Pid;
							  false -> new_session(make_mod(A#arg.appmoddata))
						 end;
					false -> new_session(make_mod(A#arg.appmoddata))
			  end,
	 Spid ! {self(), {request, R}},
	 receive
		  {Spid, Reply} ->
				 add_session(A, Spid, Reply)
	 after 5000 ->
				{ehtml, {p, [{style, "color: red"}],
							io_lib:format("Error from module ~s (timeout after 5s) session ~s",
											  [make_mod(A#arg.appmoddata), pid_to_list(Spid)])}}
	 end.
