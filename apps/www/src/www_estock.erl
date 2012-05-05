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

-record(view_data, {
		  name = "ECHO",
		  start = 0,
		  scale = day,
		  limit = 50}).

-record(add_data, {
		  name,
		  timestamp = undefined,
		  price,
		  amount
		 }).

%% for easie testing allow post and gets
out(A) ->
	Req = A#arg.req,
	Uri = yaws_api:request_url(A),
	Path = string:tokens(Uri#url.path, "/"), 
	QArgs = lists:keysort(1, yaws_api:parse_query(A)),
	Args = 
		case Req#http_request.method of
			'POST' ->
				PArgs = lists:keysort(1, yaws_api:parse_post(A)),
				{ post, lists:keymerge(1, PArgs, QArgs) };
			'GET' ->
				{ get, QArgs }
	end,
	case Path of
		["estock", "view"] -> handle_view(Args);
		["estock", "add"] -> handle_add(Args);
		_ -> handle_view(Args)
	end.

%% TODO: no validation yet, sorry.
handle_add({_Method, Args}) ->
	P = parse_add_data(Args, #add_data{}),
	if length(Args) == 0 ->
			ehtml([draw_add_form()]);
	   true ->
			R = #stock_row{
			  timestamp = case P#add_data.timestamp of
							  undefined -> 
								  datetime_util:epoch_millis();
							  T -> T
						  end,
			  name = P#add_data.name,
			  price = P#add_data.price,
			  amount = P#add_data.amount
			 },
			estockd:add_row(R),
			ehtml([
				   {p, [], "Added, thanks."}
				  ])
	end.

handle_view({_Method, Args}) ->
	P = parse_view_data(Args, #view_data{}),
	if length(Args) == 0 ->
			ehtml([
				   draw_hello(),
				   draw_view_form(P)
				  ]);
	   true ->
			ehtml([
				   draw_hello(),
				   draw_view_form(P),
				   draw_table(P)
				  ])
	end.


parse_view_data(L, P) ->
	case L of
		[] -> P;
		[I|R] -> 
			Np = case I of 
					 {"name", V} -> P#view_data{ name = V };
					 {"scale", V} -> P#view_data{ scale = V};
					 {"start", V} ->
						 {D, _} = string:to_integer(V), 
						 P#view_data{ start = D };
					 {"limit", V} -> 
						 {D, _} = string:to_integer(V), 
						 P#view_data{ limit = D };
					 _ -> P
				 end,
			parse_view_data(R, Np)
	end.

parse_add_data(L, P) ->
	case L of
		[] -> P;
		[I | R] ->
			Np = case I of 
					 {"name", V} -> P#add_data{ name = V };
					 {"timestamp", ""} -> 
						 P;
					 {"timestamp", V} -> 
						 {D, []} = string:to_integer(V),
						 P#add_data{ timestamp = D };
					 {"price", V} ->
						 {D, []} = string:to_float(V), 
						 P#add_data{ price = D };
					 {"amount", V} -> 
						 {D, []} = string:to_integer(V), 
						 P#add_data{ amount = D };
					 _ -> P
				 end,
			parse_add_data(R, Np)
	end.


draw_hello() ->
	{p, [], "Welcome to EStock service."}.

draw_view_form(PostData) ->
	{form, [{action, "/estock/view"},
			{method, "post"}],
	 [{p, [], "Instrument"},
	  {input, [{name, name}, {type, text}, {value, PostData#view_data.name}]},
	  {p, [], "Start Date"},
	  {input, [{name, start}, {type, text}, {value, PostData#view_data.start}]},
	  {p, [], "Scale"},
	  {input, [{name, scale}, {type, text}, {value, PostData#view_data.scale}]},
	  {p, [], "Limit"},
	  {input, [{name, limit}, {type, text}, {value, "50"}]},
	  {input, [{type, submit}]}]}.

draw_add_form() ->
	{form, [{action, "/estock/add"},
			{method, "post"}],
	 [{p, [], "Instrument"},
	  {input, [{name, name}, {type, text}, {value, ""}]},
	  {p, [], "Date"},
	  {input, [{name, timestamp}, {type, text}, {value, ""}]},
	  {p, [], "Price"},
	  {input, [{name, price}, {type, text}, {value, ""}]},
	  {p, [], "Amount"},
	  {input, [{name, amount}, {type, text}, {value, ""}]},
	  {input, [{type, submit}]}]}.


draw_table(PostData) ->
	Rows = [ draw_row(Agg)
			  || Agg <- estockd:list_aggs(
						  PostData#view_data.name,
						  binary_to_existing_atom(
							list_to_binary(PostData#view_data.scale),
							latin1),
						  PostData#view_data.start,
						  PostData#view_data.limit)],
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
	  {th, [], "Max"},
	  {th, [], "Amount"}]};
draw_row({{_, Scale, Timestamp}, 
		  #stock_agg {
			open_price = OP,
			close_price = CP,
			min_price = MinP,
			max_price = MaxP,
			amount = Amount
		   }}) ->
	{tr, [], [
			  {td, [], format_ts(Scale, Timestamp)},
			  {td, [], format_number(OP)},
			  {td, [], format_number(CP)},
			  {td, [], format_number(MinP)},
			  {td, [], format_number(MaxP)},
			  {td, [], integer_to_list(Amount)}
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

