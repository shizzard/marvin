-module(marvin_pdu2_object_role).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id :: id(),
    name :: name(),
    permissions :: permissions(),
    position :: position(),
    color :: color(),
    mentionable :: mentionable(),
    managed :: managed(),
    hoist :: hoist()
}).

-type id() :: marvin_pdu2:snowflake().
-type name() :: unicode:unicode_binary().
-type permissions() :: non_neg_integer().
-type position() :: non_neg_integer().
-type color() :: non_neg_integer().
-type mentionable() :: boolean().
-type managed() :: boolean().
-type hoist() :: boolean().
-type t() :: #?MODULE{}.

-export_type([
    id/0, name/0, permissions/0, position/0, color/0,
    mentionable/0, managed/0, hoist/0, t/0
]).


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(permissions, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(position, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(color, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(mentionable, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(managed, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(hoist, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    id = Id,
    name = Name,
    permissions = Permissions,
    position = Position,
    color = Color,
    mentionable = Mentionable,
    managed = Managed,
    hoist = Hoist
}) ->
    #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"permissions">> => Permissions,
        <<"position">> => Position,
        <<"color">> => Color,
        <<"mentionable">> => Mentionable,
        <<"managed">> => Managed,
        <<"hoist">> => Hoist
    }.
