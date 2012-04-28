%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Naive noise generator.
%%% @end
%%% Created : 28 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(gen_rnd).
-export([noise/2, noise_list/2]).

-include_lib("eunit/include/eunit.hrl").

random_walk(Price, Steps) ->
    Price + random_walk(Steps).
random_walk(Step) when Step =:= 0 -> 0;
random_walk(Step)->
    (random:uniform() - 0.485) * 2 + random_walk(Step-1).

noise(T, Seed) ->
    noise_sin_i(T, 1, Seed).

noise_sin_i(_, _, []) ->
    0.5;
noise_sin_i(T, Idx, [Phi | Rest]) ->
    math:sin( T * Idx + Phi ) + noise_sin_i(T, Idx + 1, Rest).

smooth_noise(X, Seed) ->
    noise(X, Seed)/2  +  noise(X-1, Seed)/4  +  noise(X + 1, Seed)/4.

-spec noise_list(integer(), integer()) -> [float()].
noise_list(N, Harmonics) ->
    Seed = [ random:uniform() * N * 3.14 || _ <- lists:seq(1, Harmonics) ],
    [ {X, smooth_noise(X / N * Harmonics * 3.14, Seed)} || X <- lists:seq(1, N) ].

%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).

price_list(_, N) when N =:= 0 -> [];
price_list(Price, N) ->
    NewPrice = random_walk(Price, 2),
    [NewPrice] ++ price_list(NewPrice, N-1).

price_list_indexed(Price, N) ->
    lists:zip(lists:seq(1, N), price_list(Price, N)).

dump_to_file(FileName, Amount, Price) ->
    {ok, F}=file:open(FileName, [read,write]),
    file:write(F, [ integer_to_list(Id) ++ " " 
		    ++ float_to_list(Val) ++ "\n"
		    || { Id, Val } <-price_list_indexed(Price, Amount) ]),
    file:close(F).

rnd_test() ->
    dump_to_file("bff.txt", 2000, 60).

-endif.
