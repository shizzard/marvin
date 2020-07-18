-module(marvin_pdu2_object_attachment).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    id = undefined :: id(),
    filename = undefined :: filename(),
    size = undefined :: size(),
    url = undefined :: url(),
    proxy_url = undefined :: proxy_url(),
    height = undefined :: height(),
    width = undefined :: width()
}).

-type id() :: marvin_pdu2:snowflake().
-type filename() :: unicode:unicode_binary().
-type size() :: pos_integer().
-type url() :: unicode:unicode_binary().
-type proxy_url() :: unicode:unicode_binary().
-type height() :: pos_integer().
-type width() :: pos_integer().
-type t() :: #?MODULE{}.

-export_type([id/0, filename/0, size/0, url/0, proxy_url/0, height/0, width/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    id = Id,
    filename = Filename,
    size = Size,
    url = Url,
    proxy_url = ProxyUrl,
    height = Height,
    width = Width
}) ->
    #{
        <<"id">> => marvin_pdu2:nullify(Id),
        <<"filename">> => marvin_pdu2:nullify(Filename),
        <<"size">> => marvin_pdu2:nullify(Size),
        <<"url">> => marvin_pdu2:nullify(Url),
        <<"proxy_url">> => marvin_pdu2:nullify(ProxyUrl),
        <<"height">> => marvin_pdu2:nullify(Height),
        <<"width">> => marvin_pdu2:nullify(Width)
    }.
