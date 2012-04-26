%%%-------------------------------------------------------------------
%%% @author Andrey Stepachev <octo@octo-laptop>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Handles requests for particual names
%%% @end
%%% Created : 26 Apr 2012 by Andrey Stepachev <octo@octo-laptop>
%%%-------------------------------------------------------------------
-module(estockd_worker).

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/1, start_worker/2, find_or_create/2, add_row/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("estockd.hrl").
-define(SERVER, ?MODULE). 

-define(MIN(A, B), if A < B -> A; true -> B end).
-define(MAX(A, B), if A > B -> A; true -> B end).
-define(OPEN_PRICE(A, ATS, B, BTS), if ATS < BTS -> {A, ATS}; true -> {B, BTS} end).
-define(CLOSE_PRICE(A, ATS, B, BTS), if ATS > BTS -> {A, ATS}; true -> {B, BTS} end).

-record(state, {name :: string(), table :: term()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec start_worker(Name :: string(), Tab :: term()) -> any().
start_worker(Name, Tab) ->
    estockd_sup:start_worker([Name, Tab]).

add_row(Pid, Row) ->
    gen_server:cast(Pid, {add_row, Row}).

-spec find_or_create(Name :: string(), Tab :: term()) -> term().
find_or_create(Name, Tab) ->
    case gproc:lookup_local_name(Name) of
	undefined ->
	    start_worker(Name, Tab),
	    case gproc:lookup_local_name(Name) of
		undefined -> erlang:error(cant_create_worker, Name);
		Addr -> Addr
	    end;
	Addr -> 
	    Addr
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Tab]) ->
    true = gproc:add_local_name(Name),
    ?DBG("Worker for ~p~n", [Name]),
    {ok, #state{name = Name, table = Tab}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_row, Row}, State) ->
    add_row_i(Row, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% stop process
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_row_i(Row, State) ->
    {{Y, M, DD}, {HH, MM, _}} = timestamp_to_datetime(Row#stock_row.timestamp),
    D = calendar:date_to_gregorian_days(Y, M, DD),
    W = calendar:iso_week_number({Y, M, DD}),
    update_agg(year, {Y}, Row, State),
    update_agg(month, {Y, M}, Row, State),
    update_agg(week, {Y, W}, Row, State),
    update_agg(day, {Y, D}, Row, State),
    update_agg(hour, {Y, D, HH}, Row, State),
    update_agg(minute, { Y, D, HH, MM}, Row, State).

update_agg(Scale, ScaleValue, Row, State) ->
    Key = {Row#stock_row.name, Scale, ScaleValue},
    NewAgg = case ets:lookup(State#state.table, Key) of
		 [Agg] ->
		     update_agg(Row, Agg);
		 [] ->
		     make_agg(Row)
	     end,
    ets:insert(State#state.table, {Key, NewAgg}),
    NewAgg.

make_agg(Row) ->
    #stock_agg {
	   open_price = Row#stock_row.price,
	   open_price_ts = Row#stock_row.timestamp,
	   close_price = Row#stock_row.price,
	   close_price_ts = Row#stock_row.timestamp,
	   min_price = Row#stock_row.price,
	   max_price = Row#stock_row.price,
	   amount = Row#stock_row.amount
	 }.


update_agg(Row, Agg) ->
    {NOP, NOPT} = ?OPEN_PRICE(Row#stock_row.price, Row#stock_row.timestamp,
			      Agg#stock_agg.open_price, Agg#stock_agg.open_price_ts),
    {NCP, NCPT} = ?CLOSE_PRICE(Row#stock_row.price, Row#stock_row.timestamp,
			      Agg#stock_agg.close_price, Agg#stock_agg.close_price_ts),
    MinPrice = ?MIN(Row#stock_row.price, Agg#stock_agg.min_price),
    MaxPrice = ?MIN(Row#stock_row.price, Agg#stock_agg.max_price),
    #stock_agg {
	   open_price = NOP,
	   open_price_ts = NOPT,
	   close_price = NCP,
	   close_price_ts = NCPT,
	   min_price = MinPrice,
	   max_price = MaxPrice,
	   amount = Agg#stock_agg.amount + Row#stock_row.amount
	       }.
					       

timestamp_to_datetime(T) ->
    calendar:now_to_universal_time({T div 1000000,T rem 1000000,0}).

