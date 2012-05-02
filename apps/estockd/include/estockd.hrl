%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @end
%%% Created : 25 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-define(WORKER_TABLE, estockd_table).

-type tool_name() :: string().
-type timestamp() :: integer().
-type price() :: number().

-record(timeunit, {
		  time :: timestamp(),
		  count :: integer()}).

-record(recent_counter, {
		  timeunits = [] :: [#timeunit{}],
		  length = 0 :: integer(),
		  maxlength = 30 :: integer(),
		  timetick = 1000 :: integer,
		  updated = 0:: timestamp()}).

-record(stock_row, {
		  timestamp :: timestamp(),
		  name :: tool_name(),
		  price :: price(),
		  amount :: integer() }).

-record(stock_agg, {
		  open_price :: price(),
		  open_price_ts :: timestamp(),
		  close_price :: price(),
		  close_price_ts :: timestamp(),
		  min_price :: price(),
		  max_price :: price(),
		  amount :: integer()
		 }).
