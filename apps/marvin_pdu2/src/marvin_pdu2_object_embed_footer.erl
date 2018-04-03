-module(marvin_pdu2_object_embed_footer).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% text    string  footer text
% icon_url    string  url of footer icon (only supports http(s) and attachments)
% proxy_icon_url  string  a proxied url of footer icon
-record(?MODULE, {
    text :: text(),
    icon_url :: icon_url(),
    proxy_icon_url :: proxy_icon_url()
}).

-type text() :: unicode:unicode_binary().
-type icon_url() :: unicode:unicode_binary().
-type proxy_icon_url() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([text/0, icon_url/0, proxy_icon_url/0, t/0]).


cloak_validate(text, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(icon_url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(proxy_icon_url, Value) when is_binary(Value) andalso Value /= <<>> ->
    {ok, Value};

cloak_validate(_, _) ->
    {error, invalid}.


export(#?MODULE{
    text = Text,
    icon_url = IconUrl,
    proxy_icon_url = ProxyIconUrl
}) ->
    #{
        <<"text">> => Text,
        <<"icon_url">> => IconUrl,
        <<"proxy_icon_url">> => ProxyIconUrl
    }.
