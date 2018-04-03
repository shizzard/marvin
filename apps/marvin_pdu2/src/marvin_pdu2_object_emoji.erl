-module(marvin_pdu2_object_emoji).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id = undefined :: id(),
    name :: name(),
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


cloak_validate(id, null) ->
    {ok, undefined};

cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(roles, Value) when is_list(Value) ->
    case lists:all(fun is_binary/1, Value) of
        true -> {ok, Value};
        false -> {error, invalid}
    end;

cloak_validate(require_colons, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(managed, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(animated, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


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
        <<"name">> => Name,
        <<"roles">> => Roles,
        <<"require_colons">> => RequireColons,
        <<"managed">> => Managed,
        <<"animated">> => Animated
    }.
