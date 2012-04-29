%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Naive noise generator.
%%% @end
%%% Created : 28 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(gen_rnd).
-export([random_walk/2, random_walk/3, random_walk/5]).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_STEPS, 6).
-define(MIN_PRICE, 3.0).
-define(MAX_PRICE, 1000.0).

%% ===================================================================
%% Public API
%% ===================================================================

-spec random_walk(float(), float()) -> float().
random_walk(Price, Volatility) ->
    random_walk(Price, Volatility, ?DEFAULT_STEPS).
-spec random_walk(float(), float(), integer(), float(), float()) -> float().
random_walk(Price, Volatility, Steps) ->
	random_walk(Price, Volatility, Steps, ?MIN_PRICE, ?MAX_PRICE).
-spec random_walk(float(), float(), integer()) -> float().
random_walk(Price, Volatility, Steps, MinPrice, MaxPrice) ->
    NewPrice = Price + random_walk_i(Steps, Volatility),
	if NewPrice < MinPrice -> MinPrice;
	   NewPrice > MaxPrice -> MaxPrice;
	   true -> NewPrice
	end.


%% ===================================================================
%% Internal functions
%% ===================================================================

random_walk_i(Step, _) when Step =:= 0 -> 0;
random_walk_i(Step, Volatility)->
    (random:uniform() - 0.5) * 2 * Volatility + random_walk_i(Step-1, Volatility).

%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).

price_list(_, N, _, _, _) when N =:= 0 -> [];
price_list(Price, N, Volatility, MinPrice, MaxPrice) ->
    NewPrice = random_walk(Price, Volatility, 6, MinPrice * 1.0, MaxPrice * 1.0),
    [NewPrice] ++ price_list(NewPrice, N-1, Volatility, MinPrice, MaxPrice).

price_list_indexed(Price, N, Volatility, MinPrice, MaxPrice) ->
    lists:zip(lists:seq(1, N), price_list(Price, N, Volatility, MinPrice, MaxPrice)).

dump_to_file(FileName, Amount, Price, Volatility, MinPrice, MaxPrice) ->
    {ok, F}=file:open(FileName, [read,write]),
    file:write(F, [ integer_to_list(Id) ++ " " 
		    ++ float_to_list(Val) ++ "\n"
		    || { Id, Val } <-price_list_indexed(Price, Amount, Volatility, MinPrice, MaxPrice) ]),
    file:close(F).

rnd_test() ->
    dump_to_file("10_vol.txt", 3500, 60, 1, 20, 120),
    dump_to_file("07_vol.txt", 3500, 60, 0.7, 50, 120),
    dump_to_file("03_vol.txt", 3500, 60, 0.3, 20, 120),
    dump_to_file("01_vol.txt", 3500, 60, 0.1, 50, 70).

-endif.
