-module(marvin_pdu2_object_embed_image).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% url string  source url of image (only supports http(s) and attachments)
% proxy_url   string  a proxied url of the image
% height  integer height of image
% width   integer width of image
-record(?MODULE, {
    url :: url(),
    proxy_url :: proxy_url(),
    height :: height(),
    width :: width()
}).

-type url() :: unicode:unicode_binary().
-type proxy_url() :: unicode:unicode_binary().
-type height() :: pos_integer().
-type width() :: pos_integer().
-type t() :: #?MODULE{}.

-export_type([url/0, proxy_url/0, height/0, width/0, t/0]).


cloak_validate(url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(proxy_url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(height, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(width, Value) when is_integer(Value) andalso Value >= 0 ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    url = Url,
    proxy_url = ProxyUrl,
    height = Height,
    width = Width
}) ->
    #{
        <<"url">> => Url,
        <<"proxy_url">> => ProxyUrl,
        <<"height">> => Height,
        <<"width">> => Width
    }.
