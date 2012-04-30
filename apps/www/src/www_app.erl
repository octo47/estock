-module(www_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
	www_sup:start_link().

start(_StartType, _StartArgs) ->
    	www_sup:start_link().

stop(_State) ->
    	ok.
