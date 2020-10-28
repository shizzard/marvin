-module(marvin_rest2).

-include_lib("marvin_log/include/marvin_log.hrl").

-export([enqueue_request/1, request/1]).

-define(is_2xx(StatusCode), ((StatusCode div 100) == 2)).
-define(is_3xx(StatusCode), ((StatusCode div 100) == 3)).
-define(is_4xx(StatusCode), ((StatusCode div 100) == 4)).
-define(is_5xx(StatusCode), ((StatusCode div 100) == 5)).

-define(response_header_limit, <<"x-ratelimit-limit">>).
-define(response_header_remaining, <<"x-ratelimit-remaining">>).
-define(response_header_reset, <<"x-ratelimit-reset">>).



%% Interface



enqueue_request(Req) ->
    marvin_rest2_queue:push_request(
        marvin_rest2_queue:queue_name(marvin_rest2_request:ratelimit_group(Req)),
        Req
    ).



-spec request(Req :: marvin_rest2_request:t()) ->
    marvin_helper_type:generic_return(
        %% TODO: fix typing
        OkRet :: {pos_integer(), {pos_integer(), non_neg_integer()}}
    ).

request(Req) ->
    ?l_debug(#{
        text => "Performing REST request",
        what => http_request,
        http_req => marvin_log:http_req_map(#{
            method => marvin_rest2_request:method(Req),
            path => marvin_rest2_request:path(Req),
            size => byte_size(marvin_rest2_request:body(Req))
        }),
        details => #{
            ratelimit_group => marvin_rest2_request:ratelimit_group(Req)
        }
    }),
    Headers = [
        {<<"user-agent">>, get_header_user_agent()},
        {<<"authorization">>, get_header_authorization()},
        {<<"content-type">>, <<"application/json">>},
        {<<"content-length">>, byte_size(marvin_rest2_request:body(Req))}
    ],
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(
        marvin_rest2_request:method(Req),
        marvin_rest2_request:url(Req),
        Headers,
        marvin_rest2_request:body(Req),
        [{pool, marvin_rest2_request:ratelimit_group(Req)}]
    ),
    handle_response_for(Req, StatusCode, RespHeaders, ClientRef).



%% Internals



-spec handle_response_for(
    Req :: marvin_rest2_request:t(),
    StatusCode :: pos_integer(),
    RespHeaders :: marvin_helper_type:proplist(Key :: binary(), Value :: binary()),
    ClientRef :: erlang:reference()
) ->
    marvin_helper_type:generic_return(
        OkRet :: {pos_integer(), {pos_integer(), non_neg_integer()}}
    ).

handle_response_for(Req, StatusCode, RespHeaders, ClientRef) ->
    Limit = binary_to_integer(proplists:get_value(?response_header_limit, RespHeaders, <<"0">>)),
    Remaining = binary_to_integer(proplists:get_value(?response_header_remaining, RespHeaders, <<"0">>)),
    Reset = binary_to_integer(proplists:get_value(?response_header_reset, RespHeaders, <<"0">>)),
    if
        not ?is_2xx(StatusCode) ->
            ?l_warning(#{
                text => "Response for REST request",
                what => http_response, result => error,
                http_ret => marvin_log:http_ret_map(#{
                    code => StatusCode,
                    path => marvin_rest2_request:path(Req)
                }),
                details => #{
                    ratelimit_group => marvin_rest2_request:ratelimit_group(Req),
                    limit => Limit, remaining => Remaining, reset => Reset
                }
            });
        true ->
            ?l_info(#{
                text => "Response for REST request",
                what => http_response, result => ok,
                http_ret => marvin_log:http_ret_map(#{
                    code => StatusCode,
                    path => marvin_rest2_request:path(Req)
                }),
                details => #{
                    ratelimit_group => marvin_rest2_request:ratelimit_group(Req),
                    limit => Limit, remaining => Remaining, reset => Reset
                }
            })
    end,
    hackney:close(ClientRef),
    {ok, {StatusCode, {Reset, Remaining}}}.



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

