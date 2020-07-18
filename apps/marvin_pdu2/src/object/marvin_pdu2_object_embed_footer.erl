-module(marvin_pdu2_object_embed_footer).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% text    string  footer text
% icon_url    string  url of footer icon (only supports http(s) and attachments)
% proxy_icon_url  string  a proxied url of footer icon
-record(?MODULE, {
    text = undefined :: text(),
    icon_url = undefined :: icon_url(),
    proxy_icon_url = undefined :: proxy_icon_url()
}).

-type text() :: unicode:unicode_binary().
-type icon_url() :: unicode:unicode_binary().
-type proxy_icon_url() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([text/0, icon_url/0, proxy_icon_url/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


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
