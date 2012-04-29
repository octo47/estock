%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Naive noise generator.
%%% @end
%%% Created : 28 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(gen_rnd).
-export([random_walk/2, random_walk/3]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec random_walk(float(), float()) -> float().
random_walk(Price, Volatility) ->
    Price + random_walk_i(6, Volatility).
-spec random_walk(float(), integer(), float()) -> float().
random_walk(Price, Steps, Volatility) ->
    Price + random_walk_i(Steps, Volatility).

%% ===================================================================
%% Internal functions
%% ===================================================================

random_walk_i(Step, _) when Step =:= 0 -> 0;
random_walk_i(Step, Volatility)->
    (random:uniform() - 0.485) * 2 * Volatility + random_walk_i(Step-1, Volatility).

%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).

price_list(_, N, _) when N =:= 0 -> [];
price_list(Price, N, Volatility) ->
    NewPrice = random_walk(Price, Volatility),
    [NewPrice] ++ price_list(NewPrice, N-1, Volatility).

price_list_indexed(Price, N, Volatility) ->
    lists:zip(lists:seq(1, N), price_list(Price, N, Volatility)).

dump_to_file(FileName, Amount, Price, Volatility) ->
    {ok, F}=file:open(FileName, [read,write]),
    file:write(F, [ integer_to_list(Id) ++ " " 
		    ++ float_to_list(Val) ++ "\n"
		    || { Id, Val } <-price_list_indexed(Price, Amount, Volatility) ]),
    file:close(F).

rnd_test() ->
    dump_to_file("10_vol.txt", 1500, 60, 1),
    dump_to_file("07_vol.txt", 1500, 60, 0.7),
    dump_to_file("03_vol.txt", 1500, 60, 0.3),
    dump_to_file("01_vol.txt", 1500, 60, 0.1).

-endif.
