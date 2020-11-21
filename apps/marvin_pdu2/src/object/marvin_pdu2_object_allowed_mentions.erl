-module(marvin_pdu2_object_allowed_mentions).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

%% This object was intentionally left partially unimplemented
-record(?MODULE, {
    replied_user = undefined :: replied_user() | undefined
}).

-type replied_user() :: boolean() | undefined.
-type t() :: #?MODULE{}.

-export_type([replied_user/0, t/0]).

cloak_validate(_, Value) when is_boolean(Value) ->
    {ok, Value}.


export(#?MODULE{
    replied_user = RepliedUser
}) ->
    #{
        <<"replied_user">> => marvin_pdu2:nullify(RepliedUser)
    }.
