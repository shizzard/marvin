-module(marvin_pdu2_object_presence).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    user = undefined :: user(),
    game = undefined :: game() | undefined,
    status = undefined :: status()
}).

-define(status_online, <<"online">>).
-define(status_offline, <<"offline">>).
-define(status_idle, <<"idle">>).
-define(status_dnd, <<"dnd">>).

-type user() :: marvin_pdu2:snowflake().
-type game() :: marvin_pdu2_object_game:t().
-type status() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([user/0, game/0, status/0, t/0]).


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(game, Value) ->
    {ok, marvin_pdu2_object_game:new(Value)};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    user = User,
    game = Game,
    status = Status
}) ->
    #{
        <<"user">> => #{<<"id">> => User},
        <<"game">> => case Game of
            undefined -> null;
            _ -> marvin_pdu2_object_game:export(Game)
        end,
        <<"status">> => Status
    }.
