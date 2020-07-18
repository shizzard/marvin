-module(marvin_pdu2_object_embed_field).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% name    string  name of the field
% value   string  value of the field
% inline  bool    whether or not this field should display inline
-record(?MODULE, {
    name = undefined :: name(),
    value = undefined :: value(),
    inline = undefined :: inline()
}).

-type name() :: unicode:unicode_binary().
-type value() :: unicode:unicode_binary().
-type inline() :: boolean().
-type t() :: #?MODULE{}.

-export_type([name/0, value/0, inline/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    name = Name,
    value = Value,
    inline = Inline
}) ->
    #{
        <<"name">> => marvin_pdu2:nullify(Name),
        <<"value">> => marvin_pdu2:nullify(Value),
        <<"inline">> => marvin_pdu2:nullify(Inline)
    }.
