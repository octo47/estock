-module(www_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).
-export([get_conf_param/2]).
-export([docroot/0, logdir/0]).

-define(DOCROOT, "./docroot").
-define(LOGDIR, "./logs").

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
	www_sup:start_link().

start(_StartType, _StartArgs) ->
    	www_sup:start_link().

stop(_State) ->
    	ok.

get_conf_param(Param, Default) ->
	case application:get_env(www, Param) of
		{ok, V} -> V;
		R -> io:format("Got ~p, Using default for ~p: ~p~n", 
					   [R, Param, Default]),
			 Default
	end.

docroot() ->	
	get_conf_param(docroot, ?DOCROOT).

logdir() ->	
	get_conf_param(logdir, ?LOGDIR).
