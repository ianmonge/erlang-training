-module(process).

-export([start/0, loop/1, send/1]).

-record(state, {data}).

start() ->
    State = #state{data = default},
    Pid = spawn(process, loop, [State]),
    register(destprocess, Pid).

loop(State) ->
    timer:sleep(1000),
    receive
        {set, data, Data} ->
           io:format("Data is ~p ~n", [State#state.data]),
           loop(State#state{data = Msg})
        {get, data} ->
            io:format("Data is ~p ~n", [State#state.data]);
        accessor ->
            io:format("Data is ~p ~n", [State#state.data]);
        terminate ->
            exit(reason);
        Msg ->
            io:format("received ~p ~n", [Msg]),
            loop(State#state{data = Msg})
    end,
    loop(State).

send(Msg) ->
    destprocess ! Msg.


%%% userId, Name, level, resources(xp, food, gold)
