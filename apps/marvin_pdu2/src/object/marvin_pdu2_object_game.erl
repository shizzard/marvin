-module(marvin_pdu2_object_game).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    type = undefined :: type(),
    state = undefined :: state(),
    name = undefined :: name(),
    id = undefined :: id(),
    timestamps = undefined :: timestamps() | undefined,
    url = undefined :: url() | undefined,
    details = undefined :: details() | undefined
}).

-define(type_playing, 0).
-define(type_streaming, 1).
-define(type_listening, 2).
-define(type_watching, 3).
-define(type_custom, 4).

-type type() :: ?type_playing | ?type_streaming | ?type_listening | ?type_watching | ?type_custom.
-type state() :: unicode:unicode_binary().
-type name() :: unicode:unicode_binary().
-type id() :: unicode:unicode_binary().
-type timestamps() :: pos_integer().
-type url() :: unicode:unicode_binary().
-type details() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([type/0, name/0, timestamps/0, url/0, details/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    type = Type,
    state = State,
    name = Name,
    id = Id,
    timestamps = Timestamps,
    url = Url,
    details = Details
}) ->
    #{
        <<"type">> => marvin_pdu2:nullify(Type),
        <<"state">> => marvin_pdu2:nullify(State),
        <<"name">> => marvin_pdu2:nullify(Name),
        <<"id">> => marvin_pdu2:nullify(Id),
        <<"timestamps">> => marvin_pdu2:nullify(Timestamps),
        <<"url">> => marvin_pdu2:nullify(Url),
        <<"details">> => marvin_pdu2:nullify(Details)
    }.
