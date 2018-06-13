-module(marvin_rest).

-export([request/1]).

-define(is_2xx(StatusCode), (StatusCode div 100) == 2).
-define(is_3xx(StatusCode), (StatusCode div 100) == 3).
-define(is_4xx(StatusCode), (StatusCode div 100) == 4).
-define(is_5xx(StatusCode), (StatusCode div 100) == 5).

-define(response_header_limit, <<"X-RateLimit-Limit">>).
-define(response_header_remaining, <<"X-RateLimit-Remaining">>).
-define(response_header_reset, <<"X-RateLimit-Reset">>).



%% Interface



request(Req) ->
    case marvin_rest_ratelimit_watchdog:reserve_slot(marvin_rest_request:ratelimit_group(Req)) of
        ok ->
            Headers = [
                {<<"user-agent">>, get_header_user_agent()},
                {<<"authorization">>, get_header_authorization()},
                {<<"content-type">>, <<"application/json">>},
                {<<"content-length">>, byte_size(marvin_rest_request:body(Req))}
            ],
            marvin_log:info(
                "Request params: Ratelimit group: '~ts'; Method '~ts'; Url '~ts'; Headers ~p; Body '~ts'",
                [marvin_rest_request:ratelimit_group(Req), marvin_rest_request:method(Req), marvin_rest_request:url(Req), Headers, marvin_rest_request:body(Req)]
            ),
            {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(
                marvin_rest_request:method(Req),
                marvin_rest_request:url(Req),
                Headers,
                marvin_rest_request:body(Req),
                [{pool, marvin_rest_request:ratelimit_group(Req)}]
            ),
            handle_response_for(marvin_rest_request:ratelimit_group(Req), StatusCode, RespHeaders, ClientRef);
        {error, {retry_after, After}} ->
            {error, {retry_after, After}}
    end.



%% Internals



handle_response_for(RatelimitGroup, StatusCode, RespHeaders, ClientRef) ->
    ok = handle_response_headers(RatelimitGroup, RespHeaders),
    hackney:close(ClientRef),
    {ok, StatusCode}.



handle_response_headers(RatelimitGroup, RespHeaders) ->
    Limit = binary_to_integer(proplists:get_value(?response_header_limit, RespHeaders, <<"0">>)),
    Remaining = binary_to_integer(proplists:get_value(?response_header_remaining, RespHeaders, <<"0">>)),
    Reset = binary_to_integer(proplists:get_value(?response_header_reset, RespHeaders, <<"0">>)),
    marvin_rest_ratelimit_watchdog:update_limits(RatelimitGroup, Remaining, Reset),
    marvin_log:info(
        "Response headers for ratelimit group '~ts': limit ~p; remaining ~p; reset ~p",
        [RatelimitGroup, Limit, Remaining, Reset]
    ),
    ok.



-spec get_header_user_agent() ->
    Ret :: binary().

get_header_user_agent() ->
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    {ok, LibraryWeb} = marvin_config:get(marvin, [system_info, library_web]),
    iolist_to_binary([LibraryName, " (", LibraryWeb, ", v", LibraryVersion, ")"]).



-spec get_header_authorization() ->
    Ret :: binary().

get_header_authorization() ->
    {ok, Token} = marvin_config:get(marvin, [discord, token]),
    iolist_to_binary(["Bot ", binary_to_list(Token)]).

