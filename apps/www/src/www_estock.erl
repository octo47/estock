%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% http requests handler for estock.
%%% @end
%%% Created :  1 May 2012 by Andrey Stepachev <octo47@gmail.com>

-module(www_estock).
-include("../../../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

-record(post_data, {
		  name = "YNDX",
		  start = datetime_util:epoch_millis(),
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
			io_lib:format("post_data: ~p~n", [P]),
			ehtml([
				   draw_hello(),
				   draw_form(P),
				   draw_table()
				  ])
	end.

parse_post_data(L, P) ->
	[I|R] = L,
	Np = case I of 
			 {"name", V} -> P#post_data{ name = V };
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
	  {p, [], "Limit"},
	  {input, [{name, limit}, {type, text}, {value, "50"}]},
	  {input, [{type, submit}]}]}.

draw_table() ->
	{p, [], "Table will be here soon..."}.

ehtml(Inner) ->
    {ehtml,
     {html,[],
	  {body, [], Inner }
	 }
	}.

