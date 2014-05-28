-module(proc_circle).

-export([start/1, loop/1, print_circle/1, send/2]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HOW-TO
%%
%% c(proc_circle).
%% FirstPid = proc_circle:start(4).
%% proc_circle:print_circle(FirstPid).
%% proc_circle:send(FirstPid, 3).
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(N) ->
    create_circle(N).

create_circle(N) ->
    FirstPid = spawn(proc_circle, loop, [undefined]),
    io:format("Created process #1 ~p~n", [FirstPid]),
    LastPid = spawn_linked_processes(N, FirstPid),
    FirstPid ! {assign_process, LastPid},
    io:format("Created link for process #1 ~p -> ~p~n", [FirstPid, LastPid]),
    FirstPid.

spawn_linked_processes(N, LinkedPid) when N > 2 ->
    CurrentPid = create_linked_process(LinkedPid, N),
    spawn_linked_processes(N-1, CurrentPid);

%% Last process created
spawn_linked_processes(2, LinkedPid) ->
    create_linked_process(LinkedPid, 2).

print_circle(FirstPid) ->
    FirstPid ! {print_link, FirstPid, 1}.

send(FirstPid, M) ->
    FirstPid ! {send, FirstPid, 1, M}.

create_linked_process(LinkedPid, Num) ->
    CurrentPid = spawn(proc_circle, loop, [LinkedPid]),
    io:format("Created linked process #~p ~p -> ~p~n", [Num, CurrentPid, LinkedPid]),
    CurrentPid.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(LinkedPid) ->
    timer:sleep(500),
    receive
        {assign_process, NewLinkedPid} ->
            loop(NewLinkedPid);

        %% Last process
        {print_link, FirstPid, N} when FirstPid =:= LinkedPid ->
            io:format("Process #~p ~p is linked to ~p~n", [N, self(), LinkedPid]),
            loop(LinkedPid);
        %% Not last process
        {print_link, FirstPid, N} ->
            io:format("Process #~p ~p is linked to ~p~n", [N, self(), LinkedPid]),
            LinkedPid ! {print_link, FirstPid, N+1},
            loop(LinkedPid);

        %% Last process in last iteration
        {send, FirstPid, N, 1} when FirstPid =:= LinkedPid ->
            io:format("Process #~p ~p sends last message #1 to ~p and dies~n", [N, self(), LinkedPid]);
        %% Not last process in last iteration
        {send, FirstPid, N, 1} ->
            io:format("Process #~p ~p sends last message #1 to ~p and dies~n", [N, self(), LinkedPid]),
            LinkedPid ! {send, FirstPid, N+1, 1};
        %% Last process in not last iteration
        {send, FirstPid, N, M} when FirstPid =:= LinkedPid ->
            io:format("Process #~p ~p sends message #~p to ~p~n", [N, self(), M, LinkedPid]),
            LinkedPid ! {send, FirstPid, 1, M-1},
            loop(LinkedPid);
        %% Not last process in not last iteration
        {send, FirstPid, N, M} ->
            io:format("Process #~p ~p sends message #~p to ~p~n", [N, self(), M, LinkedPid]),
            LinkedPid ! {send, FirstPid, N+1, M},
            loop(LinkedPid);

        Msg ->
            io:format("Process ~p receive msg '~p'~n", [self(), Msg]),
            loop(LinkedPid)
    end.
