-module(proc_star).

-export([
    start/1,
    loop_root/1,
    loop_vertex/1,
    print_star/1,
    print_star/2,
    send/2,
    send_to_vertex/2,
    send_to_vertexs/2,
    send_to_root/1,
    send_to_root_last/1
]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HOW-TO
%%
%% c(proc_star).
%% RootPid = proc_star:start(4).
%% proc_circle:print_star(RootPid).
%% proc_circle:send(RootPid, 3).
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(N) ->
    create_star(N).

create_star(N) ->
    RootPid = spawn(proc_star, loop_root, [[]]),
    io:format("Created root process ~p~n", [RootPid]),
    ListVertexs = create_vertexs(N-1, RootPid),
    RootPid ! {assign_list_vertexs, ListVertexs},
    RootPid.

create_vertexs(0, _) ->
    [];
create_vertexs(N, RootPid) ->
    VertexPid = create_process_vertex(RootPid),
    ListVertexs = create_vertexs(N-1, RootPid),
    [VertexPid | ListVertexs].

create_process_vertex(RootPid) ->
    CurrentPid = spawn(proc_star, loop_vertex, [RootPid]),
    io:format("Created vertex process ~p -> ~p~n", [CurrentPid, RootPid]),
    CurrentPid.

print_star(RootPid) ->
    RootPid ! {print_star}.

print_star(RootPid, ListVertexs) ->
    io:format("Root process ~p~n", [RootPid]),
    print_vertexs(ListVertexs).

print_vertex(Vertex) ->
    Vertex ! {print_vertex}.

print_vertexs([]) ->
    ok;
print_vertexs([Vertex|ListVertexs]) ->
    print_vertex(Vertex),
    print_vertexs(ListVertexs).

send_to_vertex(Vertex, NumMsg) ->
    io:format("Sended message from root process ~p to vertex process ~p~n", [self(), Vertex]),
    Vertex ! {send_to_root, NumMsg}.

send_to_vertexs([], _) ->
    ok;
send_to_vertexs([Vertex | ListVertex], NumMsg) ->
    send_to_vertex(Vertex, NumMsg),
    send_to_vertexs(ListVertex, NumMsg).

send_to_root(RootPid) ->
    RootPid ! {recv_from_vertex, self()},
    io:format("  Sended message from vertex process ~p to root process ~p~n", [self(), RootPid]).

send_to_root_last(RootPid) ->
    RootPid ! {recv_from_vertex_last, self()},
    io:format("  Sended last message from vertex process ~p to root process ~p~n", [self(), RootPid]).

send(RootPid, M) ->
    io:format("*** Start sending iteration ~p~n", [M]),
    RootPid ! {send_to_vertexs, M}.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop_root(ListVertexs) ->
    timer:sleep(500),
    receive
        {assign_list_vertexs, NewListVertexs} ->
            io:format("Process ~p has new list of vertexs ~p~n", [self(), NewListVertexs]),
            loop_root(NewListVertexs);

        {print_star} ->
            print_star(self(), ListVertexs),
            loop_root(ListVertexs);

        {send_to_vertexs, 1} ->
            send_to_vertexs(ListVertexs, 1),
            loop_root(ListVertexs);

        {send_to_vertexs, M} ->
            send_to_vertexs(ListVertexs, M),
            send(self(), M-1),
            loop_root(ListVertexs);

        {recv_from_vertex, Vertex} ->
            io:format("Root process ~p receive message from vertex process ~p~n", [self(), Vertex]),
            loop_root(ListVertexs);

        {recv_from_vertex_last, Vertex} ->
            io:format("Root process ~p receive last message from vertex process ~p~n", [self(), Vertex]),
            NewListVertexs = lists:delete(Vertex, ListVertexs),
            case NewListVertexs of
                [] ->
                    io:format("Root process ~p hasn't vertexs and dies~n", [self()]);
                _ ->
                    loop_root(NewListVertexs)
            end;

        Msg ->
            io:format("ERROR: Process ~p receeive msg '~p'~n", [self(), Msg]),
            loop_root(ListVertexs)
    end.

loop_vertex(RootPid) ->
    timer:sleep(500),
    receive
        {print_vertex} ->
            io:format("  Vertex process ~p~n", [self()]),
            loop_vertex(RootPid);

        {send_to_root, 1} ->
            send_to_root_last(RootPid),
            io:format("  Vertex process ~p dies~n", [self()]);
        {send_to_root, _} ->
            send_to_root(RootPid),
            loop_vertex(RootPid);

        Msg ->
            io:format("  ERROR: Process ~p receeive msg '~p'~n", [self(), Msg]),
            loop_vertex(RootPid)
    end.
