-module(marvin_pdu2_object_embed_author).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% name    string  name of author
% url string  url of author
% icon_url    string  url of author icon (only supports http(s) and attachments)
% proxy_icon_url  string  a proxied url of author icon
-record(?MODULE, {
    name = undefined :: name(),
    url = undefined :: url(),
    icon_url = undefined :: icon_url() | undefined,
    proxy_icon_url = undefined :: proxy_icon_url() | undefined
}).

-type name() :: unicode:unicode_binary().
-type url() :: unicode:unicode_binary().
-type icon_url() :: unicode:unicode_binary().
-type proxy_icon_url() :: unicode:unicode_binary().
-type t() :: #?MODULE{}.

-export_type([name/0, url/0, icon_url/0, proxy_icon_url/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    name = Name,
    url = Url,
    icon_url = IconUrl,
    proxy_icon_url = ProxyIconUrl
}) ->
    #{
        <<"name">> => Name,
        <<"url">> => Url,
        <<"icon_url">> => marvin_pdu2:nullify(IconUrl),
        <<"proxy_icon_url">> => marvin_pdu2:nullify(ProxyIconUrl)
    }.
