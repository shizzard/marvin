-module(marvin_pdu2_status_update).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% since   ?integer    unix time (in milliseconds) of when the client went idle, or null if the client is not idle
% game    ?activity object    null, or the user's new activity
% status  string  the user's new status
% afk bool    whether or not the client is afk
-record(?MODULE, {
    since = undefined :: since() | undefined,
    game = undefined :: game() | undefined,
    status = undefined :: status(),
    afk = false :: afk()
}).

-define(status_online, <<"online">>).
-define(status_offline, <<"offline">>).
-define(status_idle, <<"idle">>).
-define(status_dnd, <<"dnd">>).
-define(status_invisible, <<"invisible">>).

-type since() :: pos_integer().
-type game() :: marvin_pdu2_object_game:t().
-type status() :: unicode:unicode_binary().
-type afk() :: boolean().
-type t() :: #?MODULE{}.

-export_type([since/0, game/0, status/0, afk/0, t/0]).


%% Interface



cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(game, Value) ->
    {ok, marvin_pdu2_object_game:new(Value)};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    since = Since,
    game = Game,
    status = Status,
    afk = Afk
}) ->
    #{
        <<"since">> => marvin_pdu2:nullify(Since),
        <<"game">> => case Game of
            undefined -> null;
            _ -> marvin_pdu2_object_game:export(Game)
        end,
        <<"status">> => Status,
        <<"afk">> => Afk
    }.
