-module(marvin_dialogflow).

-export([detect_intent/3]).



%% Interface



detect_intent(GuildId, UserId, MessageBody) ->
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(post, build_url(), [
        {<<"user-agent">>, get_header_user_agent()},
        {<<"authorization">>, get_header_authorization()},
        {<<"content-type">>, <<"application/json; charset=utf-8">>}
    ], build_body(get_session_id(GuildId, UserId), MessageBody), [{pool, ?MODULE}]),
    handle_response(StatusCode, RespHeaders, ClientRef).



%% Internals



handle_response(200, _RespHeaders, ClientRef) ->
    {ok, Body} = hackney:body(ClientRef),
    Result = try
        {ok, marvin_dialogflow_response:new(jiffy:decode(Body, [return_maps]))}
    catch
        _T:Reason -> {error, Reason}
    end,
    hackney:close(ClientRef),
    Result;

handle_response(StatusCode, RespHeaders, ClientRef) ->
    marvin_log:warn(
        "Dialogflow request failed with code '~p', response headers: ~p",
        [StatusCode, RespHeaders]
    ),
    hackney:close(ClientRef),
    {error, StatusCode}.



build_url() ->
    {ok, DialogFlowRootUrl} = marvin_config:get(marvin, [dialogflow, api, root_url]),
    {ok, DialogFlowHost} = marvin_config:get(marvin, [dialogflow, api, host]),
    {ok, DialogFlowPort} = marvin_config:get(marvin, [dialogflow, api, port]),
    <<
        "https://",
        (list_to_binary(DialogFlowHost))/binary, ":",
        (list_to_binary(DialogFlowPort))/binary,
        (list_to_binary(DialogFlowRootUrl))/binary, "/query?v=20170712"
    >>.



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
