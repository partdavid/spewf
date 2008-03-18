-module(bd_server).

-behaviour(gen_server).

-include("board.hrl").

%% API
-export([
         start_link/0,
         stop/0
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {cache}).

-define(SERVER, ?MODULE).

%% @spec start_link() -> {ok, pid()} + {error, Reason}
%%    Reason = any()
%% @doc Starts the board server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stops the board server.
stop() ->
    gen_server:cast(?SERVER, stop).

init([]) ->
    {ok, #state{}}.

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
