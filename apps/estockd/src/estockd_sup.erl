
-module(estockd_sup).

-behaviour(supervisor).
-include("log.hrl").

%% API
-export([start_link/0, start_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER_SUP, estockd_worker_sup).
-define(WORKER_MODULE, estockd_worker).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Args) ->
    supervisor:start_child(?WORKER_SUP, [ Args ]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([estockd_workers]) ->
    ?DBG("Workers supervisor", []), 
    StartFunc = { ?WORKER_MODULE, start_link, [] },
    Restart = temporary,
    Shutdown = 2000, 
    Workers = { estockd_worker, StartFunc, Restart, Shutdown, worker, [?WORKER_MODULE] },
    {ok, {{simple_one_for_one, 10, 10}, [ Workers ]}};

init([]) ->
    StartFunc = {supervisor, start_link, [{local, ?WORKER_SUP}, ?MODULE, [estockd_workers]]},
    Restart = permanent,
    Shutdown = brutal_kill, 
    CoreSup ={ ?WORKER_SUP, StartFunc, Restart, Shutdown, supervisor, [?MODULE] },
    ?DBG("Core supervisor", []),
    {ok, { {one_for_one, 5, 10}, [CoreSup]} }.

