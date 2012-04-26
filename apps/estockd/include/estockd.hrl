%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @end
%%% Created : 25 Apr 2012 by Andrey Stepachev <octo47@gmail.com>

-type tool_name() :: string().
-type timestamp() :: integer().
-type price() :: number().

-record(stock_row, {
		  timestamp :: timestamp(),
		  name :: tool_name(),
		  price :: price(),
		  amount :: integer() }).

-record(stock_agg, {
		  timestamp :: timestamp(),
		  open_price :: price(),
		  close_price :: price(),
		  min_price :: price(),
		  max_price :: price(),
		  amount :: integer()
		  }).
