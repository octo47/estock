%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @end
%%% Created : 25 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(estockd).
-export([add_row/1, list_aggs/4]).

-include("estockd.hrl").

-spec add_row(#stock_row{}) -> none().
add_row(#stock_row{name=Name} = Row) ->
    Pid = estockd_worker:find_or_create(Name),
    estockd_worker:add_row(Pid, Row).

-spec list_aggs(string(), atom(), timestamp(), integer()) -> 
		      [ {timestamp(), #stock_agg{}} ].
list_aggs(Name, Scale, Start, Limit) ->
	lists:sublist(
	  estockd_worker:merge_aggs(
		estockd_server:exec_parallel(Name, 
									 estockd_worker,
									 list_aggs,
									 [Name, Scale, Start, Limit])),
	  1, Limit).
