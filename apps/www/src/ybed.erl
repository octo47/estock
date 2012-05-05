-module(ybed).
-include("../../../deps/yaws/include/yaws_api.hrl").
-export([start/0, run/0]).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    Docroot = www_app:docroot(),
    HttpPort = www_app:http_port(),
    GconfList = [{id, Id},
				 {logdir, www_app:logdir()}
				],    
    SconfList = [{port, HttpPort},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot},
                 {appmods, [{"/estock", www_estock}]}],
	io:format("Yaws config: ~p~n~p~n", [GconfList, SconfList]),
    {ok, SCList, GC, ChildSpecs} = 
		yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(www_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
