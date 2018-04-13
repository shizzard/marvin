-module(marvin_rest_request).

-callback ratelimit_group() -> atom().
-callback method() -> get | post | head | delete | patch | put | options.
-callback pdu() -> atom().
-callback url_template() -> binary().

-export([new/3, ratelimit_group/1, method/1, url/1, body/1]).

-record(?MODULE, {
    ratelimit_group :: atom(),
    method :: atom(),
    url :: binary(),
    body :: binary()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



-spec ratelimit_group(Req :: t()) ->
    Ret :: atom().

ratelimit_group(#?MODULE{ratelimit_group = Value}) ->
    Value.



-spec method(Req :: t()) ->
    Ret :: atom().

method(#?MODULE{method = Value}) ->
    Value.



-spec url(Req :: t()) ->
    Ret :: atom().

url(#?MODULE{url = Value}) ->
    Value.



-spec body(Req :: t()) ->
    Ret :: atom().

body(#?MODULE{body = Value}) ->
    Value.



-spec new(
    ReqImplModule :: atom(),
    UrlParams :: map(),
    PduMap :: map()
) ->
    Ret :: t().

new(ReqImplModule, UrlParams, PduMap) ->
    {ok, ApiRootUrl} = marvin_config:get(marvin, [discord, api, root_url]),
    PduMod = ReqImplModule:pdu(),
    #?MODULE{
        ratelimit_group = ReqImplModule:ratelimit_group(),
        method = ReqImplModule:method(),
        url = build_url(list_to_binary(ApiRootUrl), ReqImplModule:url_template(), UrlParams),
        body = jiffy:encode(PduMod:export(PduMod:new(PduMap)))
    }.



%% Internals



build_url(ApiRootUrl, UrlTemplate, UrlParams) ->
    UrlPart = maps:fold(fun build_url_fold/3, UrlTemplate, UrlParams),
    <<ApiRootUrl/binary, UrlPart/binary>>.

build_url_fold(Key, Value, UrlTemplate) ->
    binary:replace(UrlTemplate, <<"{{", Key/binary, "}}">>, Value).
