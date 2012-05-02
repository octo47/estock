%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% http requests handler for estock.
%%% @end
%%% Created :  1 May 2012 by Andrey Stepachev <octo47@gmail.com>

-module(www_estock).
-include("../../../deps/yaws/include/yaws_api.hrl").
-include("../../estockd/include/estockd.hrl").

-export([out/1]).

-record(post_data, {
		  name = "YNDX",
		  start = 0,
		  scale = day,
		  limit = 50}).

out(A) ->
	Req = A#arg.req,
	if Req#http_request.method == 'GET' ->
			ehtml([
				   draw_hello(),
				   draw_form(#post_data{})
				   ]);
	   Req#http_request.method == 'POST' ->
			L = yaws_api:parse_post(A),
			P = parse_post_data(L, #post_data{}),
			ehtml([
				   draw_hello(),
				   draw_form(P),
				   draw_table(P)
				  ])
	end.

parse_post_data(L, P) ->
	[I|R] = L,
	Np = case I of 
			 {"name", V} -> P#post_data{ name = V };
			 {"scale", V} -> P#post_data{ scale = V};
			 {"start", V} ->
				 {D, _} = string:to_integer(V), 
				 P#post_data{ start = D };
			 {"limit", V} -> 
				 {D, _} = string:to_integer(V), 
				 P#post_data{ limit = D };
			 _ -> P
		 end,
	case R of
		[] -> Np;
		_ -> parse_post_data(R, Np)
	end.

draw_hello() ->
	{p, [], "Welcome to EStock service."}.

draw_form(PostData) ->
	{form, [{action, "estock"},
			{method, "post"}],
	 [{p, [], "Instrument"},
	  {input, [{name, name}, {type, text}, {value, PostData#post_data.name}]},
	  {p, [], "Start Date"},
	  {input, [{name, start}, {type, text}, {value, PostData#post_data.start}]},
	  {p, [], "Scale"},
	  {input, [{name, scale}, {type, text}, {value, PostData#post_data.scale}]},
	  {p, [], "Limit"},
	  {input, [{name, limit}, {type, text}, {value, "50"}]},
	  {input, [{type, submit}]}]}.

draw_table(PostData) ->
	Rows = [ draw_row(Agg)
			  || Agg <- estockd:list_aggs(
						  PostData#post_data.name,
						  binary_to_existing_atom(
							list_to_binary(PostData#post_data.scale),
							latin1),
						  PostData#post_data.start,
						  PostData#post_data.limit)],
	case Rows of
		[] -> {p, [], "No data found."};
		_ -> {table, [{border, "1"}], 
			  [draw_row(header),
			   Rows
			  ]
			 }
	end.

draw_row(header)->
	{tr, [], 
	 [{th, [], "Date"}, 
	  {th, [], "Open"},
	  {th, [], "Close"},
	  {th, [], "Min"},
	  {th, [], "Max"}]};
draw_row({{_, Scale, Timestamp}, 
		  #stock_agg {
			open_price = OP,
			close_price = CP,
			min_price = MinP,
			max_price = MaxP
		   }}) ->
	{tr, [], [
			  {td, [], format_ts(Scale, Timestamp)},
			  {td, [], format_number(OP)},
			  {td, [], format_number(CP)},
			  {td, [], format_number(MinP)},
			  {td, [], format_number(MaxP)}
			 ]}.

format_ts(Scale, TimeStamp) ->
	DateTime = datetime_util:millis_to_datetime(TimeStamp),
	case Scale of
		year -> dh_date:format("Y", DateTime);
		month -> dh_date:format("F Y", DateTime);
		week -> dh_date:format("W Y", DateTime);
		day -> dh_date:format("Y, F j");
		_ -> dh_date:format("Y, F j, H:i", DateTime)
	end.

format_number(Number) ->
	if is_float(Number) ->
			io_lib:format("~.4f", [Number]);
	   is_integer(Number) ->
			io_lib:format("~.4f", [float(Number)])
	end.

ehtml(Inner) ->
    {ehtml,
     {html,[],
	  {body, [], Inner }
	 }
	}.

