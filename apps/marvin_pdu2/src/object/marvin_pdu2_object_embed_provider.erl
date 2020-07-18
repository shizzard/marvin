-module(marvin_pdu2_object_embed_provider).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% name    string  name of provider
% url string  url of provider
-record(?MODULE, {
    name = undefined :: name(),
    url = undefined :: url()
}).

-type name() :: unicode:unicode_binary().
-type url() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([name/0, url/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    name = Name,
    url = Url
}) ->
    #{
        <<"name">> => marvin_pdu2:nullify(Name),
        <<"url">> => marvin_pdu2:nullify(Url)
    }.
