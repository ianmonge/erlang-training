-module(chat).
-export([start/0]).
-export([connect/0]).
-export([send/1]).

start() ->
    spawn(fun init/0).

connect() ->
	Pid = global:whereis_name(chat_room),
    Pid ! {connect, self()},
	ok.

send(Msg) ->
	Pid = global:whereis_name(chat_room),
    Pid ! {send, self(), Msg},
	ok.

%% --

init() ->
    global:register_name(chat_room, self()),
    io:format("started~n"),
    loop([]),
    ok.

loop(Subscribers) ->
	io:format("~p~n", [Subscribers]),
    receive
        {connect, Pid} ->
		    io:format("New Conn ~p~n", [Subscribers]),
        	case lists:member(Pid, Subscribers) of
        		true ->
        			?MODULE:loop(Subscribers);
        		false ->
        			monitor(process, Pid),
        			?MODULE:loop([Pid|Subscribers])
        	end;
        {send, FromPid, Msg} ->
		    io:format("~p~n", [Subscribers]),
		    [P ! {chat, FromPid, Msg} || P <- Subscribers],
		    ?MODULE:loop(Subscribers);
		{'DOWN', _, process, Pid, _} ->
			io:format("process ~p disconnected~n", [Pid]),
			?MODULE:loop(lists:delete(Pid, Subscribers))
	end.
