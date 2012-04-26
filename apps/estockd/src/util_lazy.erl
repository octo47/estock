-module(util_lazy).

-export([seq/2, map_seq/2, map_seq/3]).

seq(M, N) when M =< N ->
    fun() -> [ M, seq(M+1, N) ] end;
seq(_, _) ->
    fun() -> [] end.

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

	    
