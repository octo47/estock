%%%-------------------------------------------------------------------
%%% @author Andrey Stepachev <octo@octo-laptop>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% This server can create a banch of processes, 
%%% which generate load on estockd server.
%%% Random Walk used.
%%% @end
%%% Created : 29 Apr 2012 by Andrey Stepachev <octo@octo-laptop>
%%%-------------------------------------------------------------------
-module(estockd_generator).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, rt_generator/1, load_generator/1]).

-include("estockd.hrl").

-define(SERVER, ?MODULE). 
-define(STEPS, 6).
-define(LOAD_FACTOR, 1000).
-define(TIMEUNIT, 1000).

-record(state, {}).

-record(rt_state, { 
		  name :: string(), 
		  price :: float(), 
		  volatility :: float(),
		  frequency :: integer(),
		  min_price :: float(),
		  max_price :: float(),
		  rps :: #recent_counter {}
		 }).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({start_rt_gen, Name, Price, Volatility, Frequency, MinPrice, MaxPrice}, _From, State) ->
    Reply = spawn_link(estockd_generator, 
					   if Frequency > 0 -> rt_generator;
						  true -> load_generator
					   end, 
					   [ #rt_state{
							name = Name,
							price = Price,
							volatility = Volatility,
							frequency = Frequency,
							min_price = MinPrice,
							max_price = MaxPrice,
							rps = #recent_counter{}
						   } 
					   ]),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_generator(State) ->
	load_generator(write_row(State)).

rt_generator(State) ->
    receive
		stop ->
			io:format("Done ~p~n", [State#rt_state.name]);
		status ->
			{_, Avg, Count} = datetime_util:aggregate_recent(State#rt_state.rps),
			io:format("Status: ~p: ~.4f of ~p ~n", 
					  [State#rt_state.name, 
					   Avg, Count]),
			rt_generator(State);
		_ ->
			io:format("Unknown message~n", []),
			rt_generator(State)
    after State#rt_state.frequency ->
			rt_generator(write_row(State))
    end.

write_row(State) ->
	Now = datetime_util:epoch_millis(),
	Price = gen_rnd:random_walk(
			  State#rt_state.price,
			  State#rt_state.volatility,
			  ?STEPS,
			  State#rt_state.min_price,
			  State#rt_state.max_price ),
	estockd:add_row(
	  #stock_row {
		 price = Price,
		 name = State#rt_state.name,
		 amount = random:uniform(100),
		 timestamp = Now
		}),
	Rps = datetime_util:update_timeunits(Now, 1, State#rt_state.rps),
	State#rt_state { price = Price, rps = Rps}.
	
