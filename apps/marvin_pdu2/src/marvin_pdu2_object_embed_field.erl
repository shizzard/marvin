-module(marvin_pdu2_object_embed_field).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% name    string  name of the field
% value   string  value of the field
% inline  bool    whether or not this field should display inline
-record(?MODULE, {
    name :: name(),
    value :: value(),
    inline :: inline()
}).

-type name() :: unicode:unicode_binary().
-type value() :: unicode:unicode_binary().
-type inline() :: boolean().
-type t() :: #?MODULE{}.

-export_type([name/0, value/0, inline/0, t/0]).


cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(value, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(inline, Value) when is_boolean(Value) ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    name = Name,
    value = Value,
    inline = Inline
}) ->
    #{
        <<"name">> => Name,
        <<"value">> => Value,
        <<"inline">> => Inline
    }.
