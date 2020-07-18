-module(marvin_pdu2_object_permission_overwrite).
-compile({parse_transform, cloak_transform}).

-export([type_member/0, type_role/0]).
-export([export/1]).

-record(?MODULE, {
    id = undefined :: id(),
    type = undefined :: type(),
    deny = undefined :: deny(),
    allow = undefined :: allow()
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

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    id = Id,
    type = Type,
    deny = Deny,
    allow = Allow
}) ->
    #{
        <<"id">> => marvin_pdu2:nullify(Id),
        <<"type">> => marvin_pdu2:nullify(Type),
        <<"deny">> => marvin_pdu2:nullify(Deny),
        <<"allow">> => marvin_pdu2:nullify(Allow)
    }.
