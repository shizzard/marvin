-module(marvin_pdu2_object_emoji).
-compile({parse_transform, cloak_transform}).

-export([export/1, format/1]).

-record(?MODULE, {
    id = undefined :: id(),
    name = undefined :: name(),
    roles = [] :: roles(),
    require_colons = false :: require_colons(),
    managed = false :: managed(),
    animated = false :: animated()
}).

-type id() :: marvin_pdu2:snowflake().
-type name() :: unicode:unicode_binary().
-type roles() :: [marvin_pdu2_object_role:id()].
-type require_colons() :: boolean().
-type managed() :: boolean().
-type animated() :: boolean().
-type t() :: #?MODULE{}.

-export_type([id/0, name/0, roles/0, require_colons/0, managed/0, animated/0, t/0]).


format(#?MODULE{id = Id, name = Name, animated = true}) ->
    <<"<a:", Name/binary, ":", Id/binary, ">">>;

format(#?MODULE{id = Id, name = Name, animated = false}) ->
    <<"<:", Name/binary, ":", Id/binary, ">">>.


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    id = Id,
    name = Name,
    roles = Roles,
    require_colons = RequireColons,
    managed = Managed,
    animated = Animated
}) ->
    #{
        <<"id">> => marvin_pdu2:nullify(Id),
        <<"name">> => marvin_pdu2:nullify(Name),
        <<"roles">> => marvin_pdu2:nullify(Roles),
        <<"require_colons">> => marvin_pdu2:nullify(RequireColons),
        <<"managed">> => marvin_pdu2:nullify(Managed),
        <<"animated">> => marvin_pdu2:nullify(Animated)
    }.
