-module(marvin_pdu2_object_reaction).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% count   integer times this emoji has been used to react
% me  bool    whether the current user reacted using this emoji
% emoji   partial emoji object    emoji information
-record(?MODULE, {
    count = undefined :: count(),
    me = undefined :: me(),
    emoji = undefined :: emoji()
}).

-type count() :: pos_integer().
-type me() :: boolean().
-type emoji() :: marvin_pdu2_object_emoji:t().
-type t() :: #?MODULE{}.

-export_type([count/0, me/0, emoji/0, t/0]).


cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(emoji, Value) ->
    {ok, marvin_pdu2_object_emoji:new(Value)};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    count = Count,
    me = Me,
    emoji = Emoji
}) ->
    #{
        <<"count">> => marvin_pdu2:nullify(Count),
        <<"me">> => marvin_pdu2:nullify(Me),
        <<"emoji">> => marvin_pdu2_object_emoji:export(Emoji)
    }.
