%%% @author Andrey Stepachev <octo47@gmail.com>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% API Server
%%% @end
%%% Created : 27 Apr 2012 by Andrey Stepachev <octo@octo-laptop>

-module(estockd_server).

-behaviour(gen_server).
-include("estockd.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([register_worker/1, find_workers/1, exec_parallel/4]).
-export([echo_callback/1]).

-define(SERVER, ?MODULE). 
-define(PIDS_TABLE, estockd_workers). 
-define(IDX_TABLE, estockd_idx). 


-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% @doc map given name to sender pid
-spec register_worker(string())->any().
register_worker(Name) ->
	gen_server:call(?SERVER, {register_worker, Name}),
	[ gen_server:call({?SERVER, Node}, {register_worker, Name}) || Node <- nodes() ].

%%% @doc lookup workers for specified Name
-spec find_workers(string())->[pid()].
find_workers(Name) ->
	case ets:lookup(?PIDS_TABLE, Name) of
		undefined -> io:format("No workers found for ~p~n", [Name]), [];
		List -> [ V || { _, V } <- List ]
	end.

%%% @doc remove pid to worker mapping
-spec unregister_worker(pid()) -> boolean().
unregister_worker(WorkerPid) ->
	case ets:lookup(?IDX_TABLE, WorkerPid) of
		[{_Pid, _Name}] = List -> 
			[ delete_worker(Name, Pid) || { Pid, Name } <- List ],
			true;
		undefined -> false
	end.

exec_parallel(Name, Module, Fun, Args) ->
	Pids = find_workers(Name),
	?DBG("Exec ~p:~p(~p) for name ~p: pids=~p~n", 
		 [Module, Fun, Args, Name, Pids]),
	if length(Pids) == 0 ->
			[];
	   true->
			Nodes = 
				gb_sets:to_list(
				  gb_sets:from_list(
					lists:map(fun(Pid) -> node(Pid) end, Pids))),
			{Good, _Bad} = gen_server:multi_call(Nodes, ?SERVER, {exec_parallel, Module, Fun, Args}),
			lists:map(fun(R) -> { _Node, Result } = R, Result end, Good)
	end.
	
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	init_tables(),
    {ok, #state{}}.

handle_call({register_worker, Name}, {Pid, _Tag}, State) ->
	erlang:monitor(process, Pid),
	save_worker(Name, Pid),
    Reply = ok,
    {reply, Reply, State};
handle_call({exec_parallel, Module, Fun, Args}, _From, State) ->
	Reply = erlang:apply(Module, Fun, Args),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
	unregister_worker(Pid),
	{noreply, State};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_tables() ->
	?DBG("Initializing tables", []),
    case ets:info(?PIDS_TABLE) of
		undefined -> ets:new(?PIDS_TABLE, [bag, named_table, protected]);
		_ -> ok %%% somehow already created
    end,
    case ets:info(?IDX_TABLE) of
		undefined -> ets:new(?IDX_TABLE, [bag, named_table, private]);
		_ -> ok %%% somehow already created
    end.

delete_worker(Name, Pid) ->
	ets:delete_object(?PIDS_TABLE, { Name, Pid }),
	ets:delete_object(?IDX_TABLE, { Pid, Name }),
	?DBG("Unregistered worker ~p for name ~p~n", [Pid, Name]).

save_worker(Name, Pid) ->
	ets:insert(?PIDS_TABLE, {Name, Pid}),
	ets:insert(?IDX_TABLE, {Pid, Name}),
	?DBG("Registered worker ~p for name ~p~n", [Pid, Name]).

echo_callback(P) ->
	io:format("Here we are, ~p~n", [P]),
	P.


%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).


dao_test() ->
	init_tables(),
	Self = self(),
	Pid2 = spawn(fun() -> receive ok -> ok end end),
	save_worker("ABC", Self),
	save_worker("CCC", Self),
	save_worker("CCC", Pid2),
	?assertEqual([Self], find_workers("ABC")),
	?assertEqual([Self, Pid2], find_workers("CCC")),
	delete_worker("ABC", self()),
	delete_worker("CCC", self()),
	[] = find_workers("ABC"),
	[Pid2] = find_workers("CCC"),
	Pid2 ! ok,
	close_tables().
	

register_test() ->
	{ok, Srv} = start_link(),
	Self = self(),
	{Pid1, _} = spawn_worker(Self, "ABC"),
	wait_ok(),
	{Pid2, _} = spawn_worker(Self, "CBA"),
	wait_ok(),

	[S1 | _Rest] = find_workers("ABC"),
	?assert(S1 =:= Pid1),

	[S2 | _Rest] = find_workers("CBA"),
	?assert(S2 =:= Pid2),

	[] = find_workers("FFF"),

	[R] = exec_parallel("ABC", estockd_server, echo_callback, ["Hello"]),
	?assertEqual(R, "Hello"),

	Pid1 ! stop,
	Pid2 ! stop,
	wait_down(),
	wait_down(),
	Srv ! stop.


wait_ok() ->
	receive ok -> ok;
			F -> io:format("Unexpected: ~p~n", [F]),
				 ?assert(F)
	end.

wait_down() ->
	receive {'DOWN', _, _, _, _} -> ok;
			F -> io:format("Unexpected: ~p~n", [F]),
				 ?assert(F)
	end.

spawn_worker(Self, Name) ->
	spawn_monitor(fun() -> 
					   register_worker(Name), 
					   Self ! ok, 
					   receive _ -> ok end 
				  end).

%%% don't know how do this better
close_tables() ->
	{Pid, _} = spawn_monitor(fun()-> receive _->ok end end),
	ets:delete_all_objects(?PIDS_TABLE),
	ets:delete_all_objects(?IDX_TABLE),
	ets:give_away(?PIDS_TABLE, Pid, {}),
	ets:give_away(?IDX_TABLE, Pid, {}),
	Pid ! ok,
	wait_down().
		
-endif.


