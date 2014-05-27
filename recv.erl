-module(recv).
-export([start/0]).
-export([start/1]).
-export([stop/1]).
-export([init/1]).
-export([increment/1]).
-export([get_value/1]).

start() ->
    spawn(?MODULE, init, [[]]).

start(StartCounter) ->
    spawn(?MODULE, init, [[StartCounter]]).

stop(Pid) ->
    cast(Pid, stop),
    ok.

init([]) ->
    loop(0);
init([StartCounter]) ->
    loop(StartCounter).

cast(Pid, Msg) ->
    Pid ! Msg,
    ok.

call(Pid, Msg) ->
    MRef = monitor(process, Pid),
    Pid ! {call, MRef, Msg},
    receive 
        {response, Ref, Value} -> 
            Value;
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, Reason} 
    after 5000 ->
        timeour
    end.

increment(Pid) ->
    cast(Pid, increment),
    ok.
    
get_value(Pid) ->
    Pid ! {get_value, self()},
    receive Value -> Value end.

loop(Counter) ->
    receive
        {get_value, FromPid} ->
            FromPid ! Counter,
            loop(Counter);
        increment ->
            io:format("~p~n", [Counter + 1]), 
            loop(Counter + 1);
        stop ->
            ok;
        Msg ->
            io:format("~p~n", [Msg]),
            loop(Counter)
    end.

