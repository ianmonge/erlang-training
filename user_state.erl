
%%% userId, Name, level, resources(xp, food, gold)

-module(user_state).

-export([start/1, loop/1, send/2, get_registered_users/0]).

-record(resources, {gold = 1000, food = 1200}).
-record(publicState, {userId, name = 'ios user', level = 1, resources = #resources{}}).

start(UserId) ->
    UserKey = get_user_key(UserId),
    case whereis(UserKey) of
        undefined ->
            UserPid = create_new_user_process(UserId),
            register_user_process(UserId, UserPid);
        UserPid ->
            UserPid %% To improve (exception error: no case clause matching <0.73.0>)
    end.

send(UserId, Msg) ->
    UserKey = get_user_key(UserId),
    case whereis(UserKey) of
        undefined ->
            UserPid = create_new_user_process(UserId),
            register_user_process(UserId, UserPid);
        UserPid ->
          UserPid %% To improve (exception error: no case clause matching <0.73.0>)
    end,
    UserPid ! Msg.

loop(PublicState) ->
    timer:sleep(1000),
    receive
        {level_up} ->
            NewLevel = PublicState#publicState.level + 1,
            io:format("Level Up to ~p~n", [NewLevel]),
            loop(PublicState#publicState{level = NewLevel});
        {rename, Name} ->
            io:format("Rename to ~p~n", [Name]),
            loop(PublicState#publicState{name = Name});
        {add_resources, ResourceType, Amount} ->
            io:format("Add ~p of ~p~n", [Amount, ResourceType]),
            NewPublicState = PublicState#publicState{resources = add_resources(PublicState, ResourceType, Amount)},
            loop(NewPublicState);
        accessor ->
            print_public_state(PublicState);
        terminate ->
            exit(reason);
        Msg ->
            io:format("UserId '~p' received '~p'~n", [PublicState#publicState.userId, Msg])
    end,
    loop(PublicState).

get_registered_users() ->
      [Pid || Pid <- registered(), nomatch =/= re:run(atom_to_list(Pid), "^u[0-9]+$")].

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_new_user_process(UserId) ->
    PublicState = #publicState{userId = UserId},
    spawn(user_state, loop, [PublicState]).

register_user_process(UserId, Pid) ->
    UserPid = get_user_key(UserId),
    register(UserPid, Pid),
    io:format("Registered new PublicState (UserId = ~p, Pid = ~p)~n", [UserId, Pid]).

get_user_key(UserId) ->
    list_to_atom("u" ++ integer_to_list(UserId)).

print_public_state(PublicState) ->
    io:nl(),
    io:format("PublicState UserId         ~p~n", [PublicState#publicState.userId]),
    io:format("PublicState Name           ~p~n", [PublicState#publicState.name]),
    io:format("PublicState Level          ~p~n", [PublicState#publicState.level]),
    Resources = PublicState#publicState.resources,
    io:format("PublicState Resources Food ~p~n", [Resources#resources.food]),
    io:format("PublicState Resources Gold ~p~n", [Resources#resources.gold]),
    io:nl().

add_resources(PublicState, ResourceType, Amount) ->
    Resources = PublicState#publicState.resources,
    case ResourceType of
        gold -> add_gold(Resources, Amount);
        food -> add_food(Resources, Amount)
    end.

add_gold(Resources, Amount) ->
    Gold = Resources#resources.gold,
    Resources#resources{gold = Gold + Amount}.

add_food(Resources, Amount) ->
    Food = Resources#resources.food,
    Resources#resources{food = Food + Amount}.
