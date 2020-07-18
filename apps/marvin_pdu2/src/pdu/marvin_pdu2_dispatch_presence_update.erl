-module(marvin_pdu2_dispatch_presence_update).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% user    user object the user presence is being updated for
% roles   array of snowflakes roles this user is in
% game    ?activity object    null, or the user's current activity
% guild_id    snowflake   id of the guild
% status  string  either "idle", "dnd", "online", or "offline"
-record(?MODULE, {
    user = undefined :: user(),
    nick = undefined :: nick(),
    roles = [] :: roles(),
    game = undefined :: game() | undefined,
    guild_id = undefined :: guild_id(),
    status = undefined :: status()
}).

-define(status_online, <<"online">>).
-define(status_offline, <<"offline">>).
-define(status_idle, <<"idle">>).
-define(status_dnd, <<"dnd">>).

-type user() :: marvin_pdu2_object_user:id().
-type nick() :: unicode:unicode_binary().
-type roles() :: [marvin_pdu2_object_role:id()].
-type game() :: marvin_pdu2_object_game:t().
-type guild_id() :: marvin_pdu2:snowflake().
-type status() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([user/0,  roles/0,  game/0,  guild_id/0,  status/0, t/0]).


%% Interface



cloak_validate(user, #{<<"id">> := Value}) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(game, Value) ->
    {ok, marvin_pdu2_object_game:new(Value)};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    user = User,
    nick = Nick,
    roles = Roles,
    game = Game,
    guild_id = GuildId,
    status = Status
}) ->
    #{
        <<"user">> => #{<<"id">> => User},
        <<"nick">> => marvin_pdu2:nullify(Nick),
        <<"roles">> => Roles,
        <<"game">> => case Game of
            undefined -> null;
            _ -> marvin_pdu2_object_game:export(Game)
        end,
        <<"guild_id">> => GuildId,
        <<"status">> => Status
    }.
