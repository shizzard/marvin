-module(marvin_rest).

-include_lib("marvin_log/include/marvin_log.hrl").

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
            ?l_info(#{
                text => "Performing REST request",
                what => http_request,
                http_req => marvin_log:http_req_map(#{
                    method => marvin_rest_request:method(Req),
                    path => marvin_rest_request:path(Req),
                    size => byte_size(marvin_rest_request:body(Req))
                }),
                details => #{
                    ratelimit_group => marvin_rest_request:ratelimit_group(Req)
                }
            }),
            Headers = [
                {<<"user-agent">>, get_header_user_agent()},
                {<<"authorization">>, get_header_authorization()},
                {<<"content-type">>, <<"application/json">>},
                {<<"content-length">>, byte_size(marvin_rest_request:body(Req))}
            ],
            {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(
                marvin_rest_request:method(Req),
                marvin_rest_request:url(Req),
                Headers,
                marvin_rest_request:body(Req),
                [{pool, marvin_rest_request:ratelimit_group(Req)}]
            ),
            handle_response_for(Req, StatusCode, RespHeaders, ClientRef);
        {error, {retry_after, After}} ->
            ?l_warning(#{
                text => "No slot available to perform REST request",
                what => http_request,
                http_req => marvin_log:http_req_map(#{
                    method => marvin_rest_request:method(Req),
                    path => marvin_rest_request:path(Req),
                    size => byte_size(marvin_rest_request:body(Req))
                }),
                details => #{
                    ratelimit_group => marvin_rest_request:ratelimit_group(Req)
                }
            }),
            {error, {retry_after, After}}
    end.



%% Internals



handle_response_for(Req, StatusCode, RespHeaders, ClientRef) ->
    Limit = binary_to_integer(proplists:get_value(?response_header_limit, RespHeaders, <<"0">>)),
    Remaining = binary_to_integer(proplists:get_value(?response_header_remaining, RespHeaders, <<"0">>)),
    Reset = binary_to_integer(proplists:get_value(?response_header_reset, RespHeaders, <<"0">>)),
    marvin_rest_ratelimit_watchdog:update_limits(marvin_rest_request:ratelimit_group(Req), Remaining, Reset),
    ?l_warning(#{
        text => "Response for REST request",
        what => http_response,
        http_ret => marvin_log:http_ret_map(#{
            code => StatusCode,
            path => marvin_rest_request:path(Req)
        }),
        details => #{
            ratelimit_group => marvin_rest_request:ratelimit_group(Req),
            limit => Limit, remaining => Remaining, reset => Reset
        }
    }),
    hackney:close(ClientRef),
    {ok, StatusCode}.



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

