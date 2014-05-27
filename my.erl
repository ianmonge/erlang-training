-module(my).
-export([f/1, fact/1, fact2/1]).

f(List) ->
	f2(List).

f([], _) ->
	[];
f([H|T], _) ->
	[do_something(H) | f(T, [])].

f2([]) ->
	[];
f2([H|T]) ->
	[do_something(H) | f2(T)].

do_something(E) ->
	E * 2.

fact(0) ->
	1;
fact(X) ->
	fact(X - 1) 
	* 
	X.

fact2(X) ->
	fact2(X, 1).

fact2(0, Acc) ->
	1 * Acc;
fact2(X, Acc) ->
	fact2(X - 1, X * Acc).
	