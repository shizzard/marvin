-module(marvin_plugin_fuzzy_nickname).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake(),
    adjectives :: [binary()],
    nouns :: [binary()]
}).
-type state() :: #state{}.

-define(message_template_placeholder_user_mention, <<"{{user_mention}}">>).

-define(noun_terms, "/voice_on_demand/noun.terms").
-define(adjective_terms, "/voice_on_demand/adjective.terms").
-define(max_channel_name_gen, 15).
-define(cleanup_event(), {cleanup_event}).
-define(cleanup_interval, 20).
-define(channel_immune_period, 30).
-define(do_rescan_category(), {do_rescan_category}).



%% Interface



-spec get_commands(L10n :: binary()) ->
    Ret :: [marvin_plugin_command:t()].

get_commands(_) ->
    [change_nickname()].



change_nickname() ->
    marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_fuzzy_nickname">>,
        command => <<"change_nickname">>,
        help => <<"Меняет твой никнейм на случайный."/utf8>>,
        keywords => [<<"ник"/utf8>>]
    }).



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(change_nickname())),
    {ok, Adjectives} = file:consult(code:priv_dir(marvin_plugin) ++ ?adjective_terms),
    {ok, Nouns} = file:consult(code:priv_dir(marvin_plugin) ++ ?noun_terms),
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId,
        adjectives = Adjectives,
        nouns = Nouns
    }}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(Info, S0) ->
    try
        handle_info_guild_event(Info, S0)
    catch
        T:R ->
            marvin_log:error(
                "Guild '~s' plugin '~s' failed guild event hadling with reason ~p:~p. ~p",
                [S0#state.guild_id, ?MODULE, T, R, erlang:get_stacktrace()]
            ),
            {noreply, S0}
    end.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



handle_info_guild_event(Event, S0) ->
    TypeCommand = marvin_guild_pubsub:type_command(),
    ShortCommandChangeNickname = marvin_plugin_command:short(change_nickname()),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeCommand, ShortCommandChangeNickname} ->
            handle_info_guild_event_change_nickname(Event, S0);
        {_Type, _Action} ->
            marvin_log:warn(
                "Plugin '~s' for guild '~s' got unknown guild event: ~ts/~ts",
                [?MODULE, S0#state.guild_id, _Type, _Action]
            ),
            {noreply, S0}
    end.



handle_info_guild_event_change_nickname(Event, S0) ->
    #{
        original_message := OriginalMessage,
        dialogflow_response := DialogFlowResponse
    } = marvin_guild_pubsub:payload(Event),
    Author = marvin_pdu2_dispatch_message_create:author(OriginalMessage),
    Nickname = generate_nickname(S0#state.adjectives, S0#state.nouns),
    marvin_log:info(
        "Plugin '~s' for guild '~s' is changing nickname for user '~s' to '~ts'",
        [?MODULE, S0#state.guild_id, marvin_pdu2_object_user:id(Author), Nickname]
    ),
    ChangeNicknameReq = marvin_rest_request:new(
        marvin_rest_impl_guild_member_update,
        #{
            <<"guild_id">> => marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event)),
            <<"user_id">> => marvin_pdu2_object_user:id(Author)
        },
        #{nick => Nickname}
    ),
    ChangeNicknameResp = marvin_rest:request(ChangeNicknameReq),
    marvin_log:info("Response: ~p", [ChangeNicknameResp]),
    SendMessageReq = marvin_rest_request:new(
        marvin_rest_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{content => fill_response_with_data(
            marvin_dialogflow_response_result:fulfillment(marvin_dialogflow_response:result(DialogFlowResponse)),
            Author
        )}
    ),
    SendMessageResp = marvin_rest:request(SendMessageReq),
    marvin_log:info("Response: ~p", [SendMessageResp]),
    {noreply, S0}.



generate_nickname(Adjectives, Nouns) ->
    Adjective = lists:nth(rand:uniform(length(Adjectives)), Adjectives),
    Noun = lists:nth(rand:uniform(length(Nouns)), Nouns),
    <<Adjective/binary, " ", Noun/binary>>.



fill_response_with_data(MessageTemplate, User) ->
    binary:replace(
        MessageTemplate,
        ?message_template_placeholder_user_mention,
        marvin_pdu2_object_user:format(User),
        [global]
    ).
