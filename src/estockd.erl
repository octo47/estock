%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @end
%%% Created : 25 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-module(estockd).
-export([add_row/1]).

-include("estockd.hrl").

add_row(#stock_row{name=Name} = Row) ->
    Pid = estockd_worker:find_or_create(Name, ?WORKER_TABLE),
    estockd_worker:add_row(Pid, Row).
