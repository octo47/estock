%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Perlin noise generator.
%%% @end
%%% Created : 28 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(gen_rnd).
-compile(export_all).

linear_interpolation(A, B, X) ->
    A * (1 - X) + B * X.

cosine_interpolation(A, B, X) ->
    Ft = X * 3.1415927,
    F = (1 - math:cos(Ft)) * 0.5,
    A*(1-F) + B*Ft.

seed() ->
    case get(noise_seed) of
	undefined ->
	    Tab = [0.58,2.79,4.55,5.94],
	    put(noise_seed, Tab),
	    Tab;
	R -> 
	    R
    end.

noise(T) ->
    noise_sin_i(T, 1, seed()).

noise_sin_i(_, _, []) ->
    0.5;
noise_sin_i(T, Idx, [Phi | Rest]) ->
    math:sin(T * Idx + Phi) + noise_sin_i(T, Idx+1, Rest).

smooth_noise(X) ->
    noise(X)/2  +  noise(X-1)/4  +  noise(X+1)/4.

-spec interpolated_noise(float())->float().
interpolated_noise(F) ->
    X = trunc(F),
    V1 = smooth_noise(X),
    V2 = smooth_noise(X+1),
    cosine_interpolation(V1, V2, F - X).

-spec perlin_noise(float())->float().
perlin_noise(F) ->
    P = 0.7071067811865475,
    perlin_noise_i(F, 0, P).

perlin_noise_i(_, I, _) when I > 6 -> 0;
perlin_noise_i(F, I, P) ->
    interpolated_noise(F * 2 * I) * P * I + perlin_noise_i(F, I + 1, P).

dump_to_file(FileName, Amount) ->
    dump_to_file(FileName, Amount, 1).
dump_to_file(FileName, Amount, Scale) ->
    {ok, F}=file:open(FileName, [read,write]),
    file:write(F, [ float_to_list(X/Scale) ++ " " 
		    ++ float_to_list(gen_rnd:perlin_noise(X/Scale)) ++ "\n"
		    || X <- lists:seq(1, Amount) ]),
    file:close(F).
