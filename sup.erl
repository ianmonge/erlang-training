-module(sup).
-export([start/0]).

start() ->
    spawn(fun init/0).

start_child() ->
    spawn_link(fun() ->
        io:format("started~n"),
        receive after 5000 -> ok end,
        io:format("stopping~n"),
        1 = 2
        end).

init() ->
    erlang:process_flag(trap_exit, true),
    loop().

loop() ->
    %% start the child process
    %% check for this process' failure
    %% if it fails, restart child process

    ChildPid = start_child(),
    receive
        {'EXIT', Pid, Reason} -> 
            io:format("pid ~p died with reason ~p~n", [Pid, Reason]),
            loop()
    end.

