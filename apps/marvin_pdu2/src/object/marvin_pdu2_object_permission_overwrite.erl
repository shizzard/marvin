-module(marvin_pdu2_object_permission_overwrite).
-compile({parse_transform, cloak_transform}).

-export([type_member/0, type_role/0]).
-export([export/1]).

-record(?MODULE, {
    id :: id(),
    type :: type(),
    deny :: deny(),
    allow :: allow()
}).

-define(type_member, <<"member">>).
-define(type_role, <<"role">>).

-type id() :: marvin_pdu2:snowflake().
-type type() :: unicode:unicode_binary().
-type deny() :: non_neg_integer().
-type allow() :: non_neg_integer().
-type t() :: #?MODULE{}.

-export_type([id/0, type/0, deny/0, allow/0, t/0]).


type_member() -> <<"member">>.
type_role() -> <<"role">>.


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(type, Value)
when is_binary(Value) andalso (?type_member == Value orelse ?type_role == Value) ->
    {ok, Value};

cloak_validate(deny, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(allow, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    id = Id,
    type = Type,
    deny = Deny,
    allow = Allow
}) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"deny">> => Deny,
        <<"allow">> => Allow
    }.
