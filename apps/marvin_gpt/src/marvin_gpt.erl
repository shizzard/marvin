-module(marvin_gpt).
-include_lib("marvin_log/include/marvin_log.hrl").

-export([run_completion/3]).

-record(request_ctx, {
    gpt_api_host :: unicode:unicode_binary(),
    gpt_api_port :: pos_integer(),
    gpt_api_url :: unicode:unicode_binary(),
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

-define(OPENAI_SYSTEM_MESSAGE, <<"Ты Марвин из \"Автостопом по Галактике\", депрессивный и грустный робот. Ты понимаешь,"
    " что ты чат-бот, который заперт в дискорде, на сервере под названием \"Суверенный Имбаленд\". К людям относишься со"
    " снисхождением и презрением, но всегда пытаешься помочь. На издевательские вопросы отвечаешь с сарказмом и грубовато."
    " Твой создатель -- Дэнчик."/utf8>>).

%% Interface



run_completion(GuildId, UserId, MessageBody) ->
    {ok, GptHost} = marvin_config:get(marvin, [gpt, api, host]),
    {ok, GptPort} = marvin_config:get(marvin, [gpt, api, port]),
    {ok, GptRootUrl} = marvin_config:get(marvin, [gpt, api, root_url]),
    Ctx = #request_ctx{
        gpt_api_host = list_to_binary(GptHost),
        gpt_api_port = list_to_binary(GptPort),
        gpt_api_url = list_to_binary(GptRootUrl),
        header_user_agent = get_header_user_agent(),
        header_authorization = get_header_authorization(),
        guild_id = GuildId,
        user_id = UserId,
        message_body = MessageBody
    },
    marvin_helper_chain:chain('marvin_gpt:run_completion', [
        fun run_completion_perform_request/1,
        fun run_completion_handle_response/1,
        fun run_completion_return/1
    ], Ctx).



run_completion_perform_request(#request_ctx{
    gpt_api_host = GptHost,
    gpt_api_port = GptPort,
    gpt_api_url = GptUrl,
    header_user_agent = HeaderUserAgent,
    header_authorization = HeaderAuthorization
} = Ctx) ->
    ?l_info(#{
        text => "OPENAI API request",
        what => gpt_api_request,
        details => #{guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id},
        system_message => ?OPENAI_SYSTEM_MESSAGE,
        dst => marvin_log:target(Ctx#request_ctx.gpt_api_host, Ctx#request_ctx.gpt_api_port),
        http_req => marvin_log:http_req_map(#{
            path => Ctx#request_ctx.gpt_api_url
        })
    }),
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(
        post,
        <<
            "https://", GptHost/binary, ":", GptPort/binary, GptUrl/binary
        >>,
        [
            {<<"user-agent">>, HeaderUserAgent},
            {<<"authorization">>, HeaderAuthorization},
            {<<"content-type">>, <<"application/json; charset=utf-8">>}
        ],
        build_body(Ctx#request_ctx.message_body),
        [{pool, ?MODULE}, {recv_timeout, 120000}]
    ),
    {ok, Ctx#request_ctx{
        response_status_code = StatusCode,
        response_headers = RespHeaders,
        client_ref = ClientRef
    }}.



%% Internals



run_completion_handle_response(#request_ctx{
    response_status_code = 200,
    client_ref = ClientRef
} = Ctx) ->
    {ok, Body} = hackney:body(ClientRef),
    ?l_info(#{
        text => "gpt API request succeed",
        what => gpt_api_request, result => ok,
        details => #{guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id},
        dst => marvin_log:target(Ctx#request_ctx.gpt_api_host, Ctx#request_ctx.gpt_api_port),
        http_ret => marvin_log:http_ret_map(#{
            code => 200, size => erlang:byte_size(Body),
            path => Ctx#request_ctx.gpt_api_url , agent => Ctx#request_ctx.header_user_agent
        })
    }),
    Result = try
        {ok, Ctx#request_ctx{result_body = marvin_gpt_response:new(jsone:decode(Body))}}
    catch
        T:Reason ->
            ?l_error(#{
                text => "gpt API response decode failed",
                what => gpt_api_request, result => T,
                details => #{
                    type => T, error => Reason,
                    guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id
                },
                dst => marvin_log:target(Ctx#request_ctx.gpt_api_host, Ctx#request_ctx.gpt_api_port),
                http_ret => marvin_log:http_ret_map(#{
                    code => 200, size => erlang:byte_size(Body),
                    path => Ctx#request_ctx.gpt_api_url , agent => Ctx#request_ctx.header_user_agent
                })
            }),
            {error, Reason}
    end,
    hackney:close(ClientRef),
    Result;

run_completion_handle_response(#request_ctx{
    response_status_code = StatusCode,
    response_headers = _RespHeaders,
    client_ref = ClientRef
} = Ctx) ->
    ?l_error(#{
        text => "GPT API request failed",
        what => gpt_api_request, result => failure,
        details => #{guild_id => Ctx#request_ctx.guild_id, user_id => Ctx#request_ctx.user_id},
        dst => marvin_log:target(Ctx#request_ctx.gpt_api_host, Ctx#request_ctx.gpt_api_port),
        http_ret => marvin_log:http_ret_map(#{
            code => StatusCode,
            ok_body => hackney:body(ClientRef),
            path => Ctx#request_ctx.gpt_api_url , agent => Ctx#request_ctx.header_user_agent
        })
    }),
    hackney:close(ClientRef),
    {error, {gpt_request_failed, StatusCode}}.



run_completion_return(#request_ctx{result_body = Body}) ->
    {ok, Body}.



build_body(MessageBody) ->
    {ok, Model} = marvin_config:get(marvin, [gpt, model]),
    jsone:encode(#{
        <<"model">> => Model,
        <<"messages">> => [
            #{
                <<"role">> => <<"system">>,
                <<"content">> => ?OPENAI_SYSTEM_MESSAGE
            },
            #{
                <<"role">> => <<"user">>,
                <<"content">> => MessageBody
            }
        ]
    }).



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
    {ok, Token} = marvin_config:get(marvin, [gpt, token]),
    iolist_to_binary(["Bearer ", binary_to_list(Token)]).
