-module(marvin_pdu2_object_embed_thumbnail).
-compile({parse_transform, cloak_transform}).

-export([export/1]).
% url string  source url of thumbnail (only supports http(s) and attachments)
% proxy_url   string  a proxied url of the thumbnail
% height  integer height of thumbnail
% width   integer width of thumbnail
-record(?MODULE, {
    url = undefined :: url(),
    proxy_url = undefined :: proxy_url(),
    height = undefined :: height(),
    width = undefined :: width()
}).

-type url() :: unicode:unicode_binary().
-type proxy_url() :: unicode:unicode_binary().
-type height() :: pos_integer().
-type width() :: pos_integer().
-type t() :: #?MODULE{}.

-export_type([url/0, proxy_url/0, height/0, width/0, t/0]).

cloak_validate(_, null) ->
    {ok, undefined};

cloak_validate(_, Value) ->
    {ok, Value}.


export(#?MODULE{
    url = Url,
    proxy_url = ProxyUrl,
    height = Height,
    width = Width
}) ->
    #{
        <<"url">> => marvin_pdu2:nullify(Url),
        <<"proxy_url">> => marvin_pdu2:nullify(ProxyUrl),
        <<"height">> => marvin_pdu2:nullify(Height),
        <<"width">> => marvin_pdu2:nullify(Width)
    }.
