-module(lazy).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


seq(M, N) when M =< N ->
    fun() -> [ M, seq(M+1, N) ] end;
seq(_, _) ->
    fun() -> [] end.

seq([]) ->
	fun() -> [] end;
seq(List) ->
	[ H | T ] = List,
    fun() -> [ H, seq(T) ] end.

map_seq(Seq, P) ->
    case Seq() of
	[Val, NextSeq] ->
	    P(Val),
	    map_seq(NextSeq, P);
	[] ->
	    ok
    end.

map_seq(M, N, P) ->
    Seq = seq(M, N),
    map_seq(Seq, P).
