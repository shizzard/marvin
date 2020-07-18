-module(marvin_pdu2_object_role).
-compile({parse_transform, cloak_transform}).

-export([export/1, format/1]).

-record(?MODULE, {
    id = undefined :: id(),
    name = undefined :: name(),
    permissions = undefined :: permissions(),
    position = undefined :: position(),
    color = undefined :: color(),
    mentionable = undefined :: mentionable(),
    managed = undefined :: managed(),
    hoist = undefined :: hoist()
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


format(#?MODULE{id = Id}) ->
    <<"<@&", Id/binary, ">">>.


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


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
        <<"id">> => marvin_pdu2:nullify(Id),
        <<"name">> => marvin_pdu2:nullify(Name),
        <<"permissions">> => marvin_pdu2:nullify(Permissions),
        <<"position">> => marvin_pdu2:nullify(Position),
        <<"color">> => marvin_pdu2:nullify(Color),
        <<"mentionable">> => marvin_pdu2:nullify(Mentionable),
        <<"managed">> => marvin_pdu2:nullify(Managed),
        <<"hoist">> => marvin_pdu2:nullify(Hoist)
    }.
