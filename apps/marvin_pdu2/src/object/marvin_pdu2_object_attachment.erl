-module(marvin_pdu2_object_attachment).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% id  snowflake   attachment id
% filename    string  name of file attached
% size    integer size of file in bytes
% url string  source url of file
% proxy_url   string  a proxied url of file
% height  ?integer    height of file (if image)
% width   ?integer    width of file (if image)
-record(?MODULE, {
    id :: id(),
    filename :: filename(),
    size :: size(),
    url :: url(),
    proxy_url :: proxy_url(),
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


cloak_validate(id, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(filename, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(size, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(proxy_url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(height, null) ->
    {ok, undefined};

cloak_validate(height, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(width, null) ->
    {ok, undefined};

cloak_validate(width, Value) when is_integer(Value) andalso Value > 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


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
        <<"id">> => Id,
        <<"filename">> => Filename,
        <<"size">> => Size,
        <<"url">> => Url,
        <<"proxy_url">> => ProxyUrl,
        <<"height">> => marvin_pdu2:nullify(Height),
        <<"width">> => marvin_pdu2:nullify(Width)
    }.
