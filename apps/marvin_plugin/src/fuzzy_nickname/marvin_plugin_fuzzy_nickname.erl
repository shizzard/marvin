-module(marvin_plugin_fuzzy_nickname).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
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
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(Info, S0) ->
    try
        handle_info_guild_event(Info, S0)
    catch
        T:R:S ->
            ?l_error(#{
                text => "Plugin failed guild event handling",
                what => handle_info, result => fail,
                details => #{
                    type => T, reason => R, stacktrace => S,
                    guild_id => S0#state.guild_id
                }
            }),
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
        {Type, Action} ->
            ?l_warning(#{
                text => "Plugin got unknown guild event",
                what => handle_info, result => fail,
                details => #{
                    pubsub_type => Type, pubsub_action => Action,
                    guild_id => S0#state.guild_id
                }
            }),
            {noreply, S0}
    end.



handle_info_guild_event_change_nickname(Event, S0) ->
    #{original_message := OriginalMessage} = marvin_guild_pubsub:payload(Event),
    Author = marvin_pdu2_dispatch_message_create:author(OriginalMessage),
    Nickname = generate_nickname(S0#state.adjectives, S0#state.nouns),
    ?l_debug(#{
        text => "Plugin is changing nickname for user",
        what => handle_info,
        details => #{
            user_id => marvin_pdu2_object_user:id(Author),
            nickname => Nickname,
            guild_id => S0#state.guild_id
        }
    }),
    ChangeNicknameReq = marvin_rest2_request:new(
        marvin_rest2_impl_guild_member_update,
        #{
            <<"guild_id">> => marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event)),
            <<"user_id">> => marvin_pdu2_object_user:id(Author)
        },
        #{nick => Nickname}
    ),
    _ = marvin_rest2:request(ChangeNicknameReq),
    SendMessageReq = marvin_rest2_request:new(
        marvin_rest2_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{content => <<"Буду звать тебя "/utf8, (marvin_pdu2_object_user:format(Author))/binary, "."/utf8>>}
    ),
    _ = marvin_rest2:request(SendMessageReq),
    {noreply, S0}.



generate_nickname(Adjectives, Nouns) ->
    Adjective = lists:nth(rand:uniform(length(Adjectives)), Adjectives),
    Noun = lists:nth(rand:uniform(length(Nouns)), Nouns),
    <<Adjective/binary, " ", Noun/binary>>.
