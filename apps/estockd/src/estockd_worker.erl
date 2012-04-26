%%%-------------------------------------------------------------------
%%% @author Andrey Stepachev <octo@octo-laptop>
%%% @copyright (C) 2012, Andrey Stepachev
%%% @doc
%%% Handles requests for particual names
%%% @end
%%% Created : 26 Apr 2012 by Andrey Stepachev <octo@octo-laptop>
%%%-------------------------------------------------------------------
-module(estockd_worker).

-behaviour(gen_server).

-include("log.hrl").

%% API
-export([start_link/1, start_worker/2, find_or_create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {name :: string(), table :: term()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec start_worker(Name :: string(), Tab :: term()) -> any().
start_worker(Name, Tab) ->
    estockd_sup:start_worker([Name, Tab]).

-spec find_or_create(Name :: string(), Tab :: term()) -> term().
find_or_create(Name, Tab) ->
    case gproc:lookup_local_name(Name) of
	undefined ->
	    start_worker(Name, Tab),
	    gproc:lookup_local_name(Name);
	Addr -> 
	    Addr
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Tab]) ->
    true = gproc:add_local_name(Name),
    ?DBG("Worker for ~p~n", [Name]),
    {ok, #state{name = Name, table = Tab}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%% stop process
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
