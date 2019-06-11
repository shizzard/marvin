-module(marvin_dialogflow).
-include_lib("marvin_log/include/marvin_log.hrl").

-export([detect_intent/3]).

-record(request_ctx, {
    dialogflow_api_host :: unicode:unicode_binary(),
    dialogflow_api_port :: pos_integer(),
    dialogflow_api_url :: unicode:unicode_binary(),
    header_user_agent :: unicode:unicode_binary(),
    header_authorization :: unicode:unicode_binary(),
    guild_id :: unicode:unicode_binary(),
    user_id :: unicode:unicode_binary(),
    message_body :: unicode:unicode_binary(),
    response_status_code :: pos_integer(),
    response_headers :: [binary()],
    client_ref :: erlang:ref(),
    result_body :: maps:map()
}).



%% Interface



detect_intent(GuildId, UserId, MessageBody) ->
    {ok, DialogFlowHost} = marvin_config:get(marvin, [dialogflow, api, host]),
    {ok, DialogFlowPort} = marvin_config:get(marvin, [dialogflow, api, port]),
    {ok, DialogFlowRootUrl} = marvin_config:get(marvin, [dialogflow, api, root_url]),
    Ctx = #request_ctx{
        dialogflow_api_host = list_to_binary(DialogFlowHost),
        dialogflow_api_port = list_to_binary(DialogFlowPort),
        dialogflow_api_url = <<(list_to_binary(DialogFlowRootUrl))/binary, "/query?v=20170712">>,
        header_user_agent = get_header_user_agent(),
        header_authorization = get_header_authorization(),
        guild_id = GuildId,
        user_id = UserId,
        message_body = MessageBody
    },
    marvin_helper_chain:chain('marvin_dialogflow:detect_intent', [
        fun detect_intent_perform_request/1,
        fun detect_intent_handle_response/1,
        fun detect_intent_return/1
    ], Ctx).



detect_intent_perform_request(#request_ctx{
    dialogflow_api_host = DialogFlowHost,
    dialogflow_api_port = DialogFlowPort,
    dialogflow_api_url = DialogFlowUrl,
    header_user_agent = HeaderUserAgent,
    header_authorization = HeaderAuthorization
} = Ctx) ->
    ?l_info(#{
        text => "Dialogflow API request",
        what => dialogflow_api_request,
        details => #{guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id},
        dst => marvin_log:target(Ctx#request_ctx.dialogflow_api_host, Ctx#request_ctx.dialogflow_api_port),
        http_req => marvin_log:http_req_map(#{
            path => Ctx#request_ctx.dialogflow_api_url
        })
    }),
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(
        post,
        <<
            "https://", DialogFlowHost/binary, ":", DialogFlowPort/binary, DialogFlowUrl/binary
        >>,
        [
            {<<"user-agent">>, HeaderUserAgent},
            {<<"authorization">>, HeaderAuthorization},
            {<<"content-type">>, <<"application/json; charset=utf-8">>}
        ],
        build_body(
            get_session_id(Ctx#request_ctx.guild_id, Ctx#request_ctx.user_id),
            Ctx#request_ctx.message_body
        ),
        [{pool, ?MODULE}]
    ),
    {ok, Ctx#request_ctx{
        response_status_code = StatusCode,
        response_headers = RespHeaders,
        client_ref = ClientRef
    }}.



%% Internals



detect_intent_handle_response(#request_ctx{
    response_status_code = 200,
    client_ref = ClientRef
} = Ctx) ->
    {ok, Body} = hackney:body(ClientRef),
    ?l_info(#{
        text => "Dialogflow API request succeed",
        what => dialodflow_api_request, result => ok,
        details => #{guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id},
        dst => marvin_log:target(Ctx#request_ctx.dialogflow_api_host, Ctx#request_ctx.dialogflow_api_port),
        http_ret => marvin_log:http_ret_map(#{
            code => 200, size => erlang:byte_size(Body),
            path => Ctx#request_ctx.dialogflow_api_url , agent => Ctx#request_ctx.header_user_agent
        })
    }),
    Result = try
        {ok, Ctx#request_ctx{result_body = marvin_dialogflow_response:new(jiffy:decode(Body, [return_maps]))}}
    catch
        T:Reason ->
            ?l_error(#{
                text => "Dialogflow API response decode failed",
                what => dialodflow_api_request, result => T,
                details => #{
                    type => T, error => Reason,
                    guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id
                },
                dst => marvin_log:target(Ctx#request_ctx.dialogflow_api_host, Ctx#request_ctx.dialogflow_api_port),
                http_ret => marvin_log:http_ret_map(#{
                    code => 200, size => erlang:byte_size(Body),
                    path => Ctx#request_ctx.dialogflow_api_url , agent => Ctx#request_ctx.header_user_agent
                })
            }),
            {error, Reason}
    end,
    hackney:close(ClientRef),
    Result;

detect_intent_handle_response(#request_ctx{
    response_status_code = StatusCode,
    response_headers = _RespHeaders,
    client_ref = ClientRef
} = Ctx) ->
    ?l_error(#{
        text => "Dialogflow API request failed",
        what => dialodflow_api_request, result => failure,
        details => #{guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id},
        dst => marvin_log:target(Ctx#request_ctx.dialogflow_api_host, Ctx#request_ctx.dialogflow_api_port),
        http_ret => marvin_log:http_ret_map(#{
            code => StatusCode,
            path => Ctx#request_ctx.dialogflow_api_url , agent => Ctx#request_ctx.header_user_agent
        })
    }),
    hackney:close(ClientRef),
    {error, {dialogflow_request_failed, StatusCode}}.



detect_intent_return(#request_ctx{result_body = Body}) ->
    {ok, Body}.



get_session_id(GuildId, UserId) ->
    iolist_to_binary([
        io_lib:format("~2.16.0b", [X]) || <<X:8>>
        <= crypto:hash(md5, <<GuildId/binary, "/", UserId/binary>>)
    ]).



build_body(SessionId, MessageBody) ->
    jiffy:encode(#{<<"lang">> => <<"ru">>, <<"query">> => MessageBody, <<"sessionId">> => SessionId}).



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
    {ok, Token} = marvin_config:get(marvin, [dialogflow, token]),
    iolist_to_binary(["Bearer ", binary_to_list(Token)]).
