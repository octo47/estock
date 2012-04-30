%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @end
%%% Created : 25 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(estockd).
-export([add_row/1, list_aggs/5]).

-include("estockd.hrl").

-spec add_row(#stock_row{}) -> none().
add_row(#stock_row{name=Name} = Row) ->
    Pid = estockd_worker:find_or_create(Name),
    estockd_worker:add_row(Pid, Row).

-spec list_aggs(string(), atom(), timestamp(), timestamp(), integer()) -> 
		      [ {timestamp(), #stock_agg{}} ].
list_aggs(Name, Scale, Start, End, Limit) ->
    case estockd_worker:find(Name) of
		undefined -> [];
		Pid -> estockd_worker:list_aggs(Pid, Scale, Start, End, Limit)
    end.
