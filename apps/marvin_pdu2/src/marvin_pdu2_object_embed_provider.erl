-module(marvin_pdu2_object_embed_provider).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% name    string  name of provider
% url string  url of provider
-record(?MODULE, {
    name :: name(),
    url :: url()
}).

-type name() :: unicode:unicode_binary().
-type url() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([name/0, url/0, t/0]).


cloak_validate(name, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    name = Name,
    url = Url
}) ->
    #{
        <<"name">> => Name,
        <<"url">> => Url
    }.
