%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% http requests handler for estock.
%%% @end
%%% Created :  1 May 2012 by Andrey Stepachev <octo47@gmail.com>

-module(www_estock).
-include("../../../deps/yaws/include/yaws_api.hrl").

-export([out/1]).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("A#arg.appmoddata = ~p~n"
                         "A#arg.appmod_prepath = ~p~n"
                         "A#arg.querydata = ~p~n",
                         [A#arg.appmoddata,
                          A#arg.appmod_prepath,
                          A#arg.querydata]))}]}.
