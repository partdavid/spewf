%%
%% "said" module by partdavid@gmail.com
%% Implements the arc challenge - http://www.paulgraham.com/arcchallenge.html
%%

-module(said).
-export([handle/2]).

-record(said, {said = null}).

handle([], R) ->
	 handle(#said{}, R);
handle(S, [{said, Value}|_]) ->
	 {S#said{said = Value}, {ehtml, {a, [{href, self}], "click me"}}};
handle(S, [_|R]) ->
	 handle(S, R);
handle(S = #said{said = null}, []) ->
	 {S, {ehtml, {form, [{action, self}],
					  [{input, [{type, "text"},
									{name, "said"}, {size, 50}], []},
						{input, [{type, "submit"}]}]}}};
handle(S = #said{said = Said}, []) ->
	 {S, {ehtml, {p, [], io_lib:format("you said: ~s", [Said])}}}.
