-module(eradius_dict).
%%%----------------------------------------------------------------------
%%% File    : eradius_dict.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : Radius dictionary handling.
%%% Created : 25 Sep 2003 by Torbjorn Tornkvist <tobbe@bluetail.com>
%%%----------------------------------------------------------------------
-behaviour(gen_server).

%% External exports
-export([
         start/0,start_link/0,
         lookup/1
        ]).
-export([load_tables/1]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include("eradius_dict.hrl").

-define(SERVER    , ?MODULE).
-define(TABLENAME , ?MODULE).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

lookup(Id) ->
    ets:lookup(?TABLENAME, Id).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

load_tables(Tables) ->
    gen_server:call(?SERVER, {load_tables, Tables}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLENAME, [named_table, {keypos, 2}, public]),
    {ok, #state{}}.

handle_call({load_tables, Tables}, _From, State) ->
    Res = (catch lists:foreach(fun(Tab) -> load_table(Tab) end, Tables)),
    {reply, Res, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% Load dictionary table
%%% --------------------------------------------------------------------

load_table(Table) ->
    Dir = priv_dir(),
    MapFile = Dir ++ "/" ++ Table ++ ".map",
    case file:consult(MapFile) of
	{ok, Res} ->
	    lists:foreach(fun(R) -> ets:insert(?TABLENAME, R) end, Res),
	    ok;
	_Error ->
	    {error, load_table}
    end.

priv_dir() ->
    P = code:which(?MODULE),
    [_,_|R] = lists:reverse(string:tokens(P,"/")),
    lists:foldl(fun(X,Acc) -> Acc ++ [$/|X] end, "", lists:reverse(R)) ++ "/priv".
