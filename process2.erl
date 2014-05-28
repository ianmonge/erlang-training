-module(process2).

-export([start/0, loopSend/1, loopRecv/0]).

start() ->
    Pid1 = spawn(process2, loopSend, [5]),
    io:format("Created sender process ~p~n", [Pid1]),
    Pid2 = spawn(process2, loopRecv, []),
    io:format("Created receiver process ~p~n", [Pid2]),
    register(p1, Pid1),
    register(p2, Pid2).

loopSend(Num) ->
    timer:sleep(1000),
    Pid2 = whereis(p2),
    case Num of
        0 ->
            Pid2 ! terminate;
        _ ->
            Pid2 ! {self(), 'hola'},
            loopSend(Num - 1)
    end.

loopRecv() ->
    receive
        {Rem, Msg} ->
            io:format("Message received from ~p '~p'~n", [Rem, Msg]),
            Rem ! {self(), recv}
            loopRecv();
        Other ->
            ok
    end.


2º ejercicio
N processos en círculo envían M mensajes en cadena

3º ejercicio
Idem a 2º pero en estrella
