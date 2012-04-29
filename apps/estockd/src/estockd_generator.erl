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
		 terminate/2, code_change/3, rt_generator/1]).

-include("estockd.hrl").

-define(SERVER, ?MODULE). 
-define(STEPS, 6).

-record(state, {}).
-record(rt_state, { 
		  name :: string(), 
		  price :: float(), 
		  volatility :: float(),
		  frequency :: integer(),
		  min_price :: float(),
		  max_price :: float()
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
    Reply = spawn_link(estockd_generator, rt_generator, 
					   [ #rt_state{
							name = Name,
							price = Price,
							volatility = Volatility,
							frequency = Frequency,
							min_price = MinPrice,
							max_price = MaxPrice
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

rt_generator(State) ->
    receive
		stop ->
			io:format("Done ~p~n", [State#rt_state.name]);
		_ ->
			io:format("Unknown message~n", []),
			rt_generator(State)
    after State#rt_state.frequency ->
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
				 timestamp = datetime_util:epoch_millis()
				}),
			rt_generator(State#rt_state {price = Price})
    end.
