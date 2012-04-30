%%%-------------------------------------------------------------------
%%% @author Andrey Stepachev <octo@octo-laptop>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Handles requests for particual names.
%%% Data stored in ets table (ordered) with key
%%% { Name, Scale, StartDateOfScale }.
%%% So, we can read aggregated rows iterating keys, 
%%% starting from first date, passed. 
%%% @end
%%% Created : 26 Apr 2012 by Andrey Stepachev <octo@octo-laptop>
%%%-------------------------------------------------------------------
-module(estockd_worker).

-behaviour(gen_server).

-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(datetime_util, [datetime_to_millis/1,
						millis_to_datetime/1,
						truncate_datetime_millis/2]).
%% API
-export([start_link/1, start_worker/2, find_or_create/1, find/1, add_row/2, list_aggs/5, init_table/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-include("estockd.hrl").
-define(SERVER, ?MODULE). 

-define(MIN(A, B), if A < B -> A; true -> B end).
-define(MAX(A, B), if A > B -> A; true -> B end).
-define(OPEN_PRICE(A, ATS, B, BTS), if ATS < BTS -> {A, ATS}; true -> {B, BTS} end).
-define(CLOSE_PRICE(A, ATS, B, BTS), if ATS > BTS -> {A, ATS}; true -> {B, BTS} end).
-define(ALL_SCALES, [year, month, week, day, hour, minute]).
-record(state, {name :: string(), table :: term()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec start_worker(Name :: string(), Tab :: term()) -> any().
start_worker(Name, Tab) ->
    estockd_sup:start_worker([Name, Tab]).

init_table() ->
    ?DBG("Initializing table", []),
    case ets:info(?WORKER_TABLE) of
		undefined -> ets:new(?WORKER_TABLE, [ordered_set, named_table, public]);
		_ -> ok %%% somehow already created
    end.

add_row(Pid, Row) ->
    gen_server:cast(Pid, {add_row, Row}).

list_aggs(Pid, Scale, Start, End, Limit) ->
    gen_server:call(Pid, {list_aggs, Scale, Start, End, Limit}).

-spec find(Name :: string()) -> term().
find(Name) ->
    gproc:lookup_local_name(Name).

-spec find_or_create(Name :: string()) -> term().
find_or_create(Name) ->
    case find(Name) of
		undefined ->
			start_worker(Name, ?WORKER_TABLE),
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

handle_call({list_aggs, Scale, Start, End, Limit}, _From, State) ->
    {reply, list_aggs_i(Scale, Start, End, Limit, State), State};
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

list_aggs_i(Scale, Start, End, Limit, State) ->
    StartKey = make_key(Scale, State#state.name, Start),
    EndKey =  make_key(Scale, State#state.name, End),
    iter_table_start(StartKey, EndKey, Limit).

iter_table_start(StartKey, EndKey, Limit) ->
    Lookup = ets:lookup(?WORKER_TABLE, StartKey),
    case Lookup of
		[{K, _}] -> lookup(K) ++ iter_table(K, EndKey, Limit-1);
		[] -> iter_table(StartKey, EndKey, Limit)
    end.

iter_table(_, _, Limit) 
  when Limit < 1 ->
    [];
iter_table(NextKey, EndKey, Limit) ->
    case ets:next(?WORKER_TABLE, NextKey) of
		'$end_of_table' -> [];
		K when K >= EndKey -> [];
	    K -> lookup(K) ++ iter_table(K, EndKey, Limit-1)
    end.

lookup(K) ->
	ets:lookup(?WORKER_TABLE, K).


add_row_i(Row, State) ->
    Timestamp = Row#stock_row.timestamp,
    Name = Row#stock_row.name,
    [ update_agg(make_key(X, Name, Timestamp), Row, State) 
      || X <- ?ALL_SCALES ],
    ok.

make_key(Scale, Name, Timestamp) ->
    Truncated = truncate_datetime_millis(Scale, millis_to_datetime(Timestamp)),
    {Name, Scale, Truncated}.

update_agg(Key, Row, State) ->
    NewAgg = case ets:lookup(State#state.table, Key) of
				 [{Key, Agg}] ->
					 update_agg(Agg, Row);
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

update_agg(Agg, Row) ->
    {NOP, NOPT} = ?OPEN_PRICE(Row#stock_row.price, Row#stock_row.timestamp,
							  Agg#stock_agg.open_price, Agg#stock_agg.open_price_ts),
    {NCP, NCPT} = ?CLOSE_PRICE(Row#stock_row.price, Row#stock_row.timestamp,
							   Agg#stock_agg.close_price, Agg#stock_agg.close_price_ts),
    MinPrice = ?MIN(Row#stock_row.price, Agg#stock_agg.min_price),
    MaxPrice = ?MAX(Row#stock_row.price, Agg#stock_agg.max_price),
    #stock_agg {
				 open_price = NOP,
				 open_price_ts = NOPT,
				 close_price = NCP,
				 close_price_ts = NCPT,
				 min_price = MinPrice,
				 max_price = MaxPrice,
				 amount = Agg#stock_agg.amount + Row#stock_row.amount 
    	       }.

%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).


%% TODO: write real test here
iter_test() ->
    init_table(),
    State = {state, "MSFT", ?WORKER_TABLE},
    Changes = [
			   {{{2011,2,15},{22,14,44}}, 25, 100}, 
			   {{{2011,2,15},{22,15,44}}, 26, 200},
			   {{{2011,2,15},{22,16,44}}, 24, 100},
			   {{{2011,2,16},{22,14,44}}, 30, 200},
			   {{{2011,3,15},{22,14,44}}, 30, 100}
			  ],
    Rows = [ #stock_row { timestamp = datetime_to_millis(Date),
						  name = "MSFT",
						  price = Price,
						  amount = Amount } 
			 || { Date, Price, Amount } <- Changes
		   ],
    [ add_row_i(R, State) || R <- Rows ],
    [
     begin
		 [ { StartDate, _, _ } | _ ] = Changes,
		 Start = datetime_to_millis(StartDate) - 1,
		 io:format("=================== ~p~n", [S]),
		 [ io:format("~p~n", [X]) 
		   || X <- list_aggs_i(S, Start, Start * 2, 1000, State)
		 ] 
     end
	 || S <- ?ALL_SCALES ].

make_key_test() ->
    Timestamp = datetime_to_millis({{2011,2,15},{22,14,44}}),
    Keys = [ begin Key = {Name, InKeyScale, _} = make_key(Scale, "ABC", Timestamp),
				   ?assert(Name =:= "ABC"),
				   ?assert(Scale =:= InKeyScale),
				   ?assert(is_integer(Timestamp)),
				   ?assert(Timestamp > 0),
				   Key
			 end || Scale <- ?ALL_SCALES ],
    io:format("Keys ~p~n", [Keys]).

make_agg_test() ->
    Row = #stock_row { timestamp = datetime_to_millis({{2011,2,15},{22,14,44}}),
					   name = "MSFT",
					   price = 25,
					   amount = 1230 },
    Agg = make_agg(Row),
    ?assert(Agg#stock_agg.amount =:= 1230),
    ?assert(Agg#stock_agg.min_price =:= 25),
    ?assert(Agg#stock_agg.max_price =:= 25),

    Row2 = #stock_row { timestamp = datetime_to_millis({{2011,2,15},{22,15,01}}),
						name = "MSFT",
						price = 26,
						amount = 123 },
    Agg2 = update_agg(Agg, Row2),
    io:format("~p~n~p~n", [Agg, Agg2]),
    ?assert(Agg2#stock_agg.amount =:= 1353),
    ?assert(Agg2#stock_agg.min_price =:= 25),
    ?assert(Agg2#stock_agg.max_price =:= 26),
    ok.

compare_agg_test() ->
	Ets = 1296518400000*2,
	E = {"MSFT", month, Ets},
	K = {"MSFT", week, 1297641600000},
	io:format("~p~p~n", [make_key(month, "MSFT", Ets), (K < E)]).

-endif.

