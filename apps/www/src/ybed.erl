-module(ybed).
-include("../../../deps/yaws/include/yaws_api.hrl").
-export([start/0, run/0]).

-define(DOCROOT, "./docroot").

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    Docroot = ?DOCROOT,
    GconfList = [{id, Id}],    
    SconfList = [{port, 7000},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot},
                 {appmods, []}],
    {ok, SCList, GC, ChildSpecs} = 
		yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(www_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
