-module(marvin_plugin_huify).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(handle_info_guild_event_command_huify, {
    guild_id :: marvin_pdu2:snowflake(),
    event :: marvin_guild_pubsub:t(),
    allowed_channel_ids :: [marvin_pdu2:snowflake()],
    original_message :: marvin_pdu2_dispatch_message_create:t(),
    parsed_message_content :: [term()],
    word :: unicode:unicode_binary(),
    message :: unicode:unicode_binary()
}).

-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake(),
    channel_counters :: map()
}).
-type state() :: #state{}.



%% Interface



-spec get_commands(L10n :: binary()) ->
    Ret :: [marvin_plugin_command:t()].

get_commands(_) ->
    [command_huify()].



command_huify() ->
    marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_huify">>,
        command => <<"huify">>,
        help => <<"Кхмфикация слова."/utf8>>,
        keywords => [<<"кхм"/utf8>>]
    }).



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_message(), marvin_guild_pubsub:action_create()),
    [
        marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(Command))
        || Command <- get_commands(any)
    ],
    #{<<"channel_ids">> := ChannelIds} = marvin_plugin_config:data(PluginConfig),
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId,
        channel_counters = maps:from_list([{ChannelId, 0} || ChannelId <- ChannelIds])
    }}.



%% Internals



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



handle_info_guild_event(Event, S0) ->
    TypeMessage = marvin_guild_pubsub:type_message(),
    TypeCommand = marvin_guild_pubsub:type_command(),
    ActionCreate = marvin_guild_pubsub:action_create(),
    ShortCommandHiufy = marvin_plugin_command:short(command_huify()),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeMessage, ActionCreate} ->
            handle_info_guild_event_message_create(Event, S0);
        {TypeCommand, ShortCommandHiufy} ->
            handle_info_guild_event_command_huify(Event, S0);
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



handle_info_guild_event_message_create(Event, S0) ->
    OriginalMessage = marvin_guild_pubsub:payload(Event),
    #{<<"channel_ids">> := ChannelIds} = marvin_plugin_config:data(S0#state.config),
    ChannelId = marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
    case lists:member(ChannelId, ChannelIds) of
        true ->
            handle_info_guild_event_message_create_handle_counter(Event, S0);
        false ->
            {noreply, S0}
    end.



%% TODO refactor this shit, omfg
handle_info_guild_event_message_create_handle_counter(Event, S0) ->
    OriginalMessage = marvin_guild_pubsub:payload(Event),
    ChannelId = marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
    #{
        <<"channel_ids">> := ChannelIds,
        <<"trigger_min">> := TriggerMin,
        <<"trigger_max">> := TriggerMax
    } = marvin_plugin_config:data(S0#state.config),
    case S0 of
        #state{channel_counters = #{ChannelId := Count} = ChannelCounters}
        when Count >= TriggerMin ->
            case {Prob = 1 / (TriggerMax - TriggerMin), rand:uniform()} of
                {A, B} when A =< B ->
                    ?l_info(#{
                        text => "Plugin is triggering",
                        what => handle_info, result => ok,
                        details => #{
                            guild_id => S0#state.guild_id,
                            probability => Prob
                        }
                    }),
                    case marvin_helper_chain:chain(
                        'marvin_plugin_huify:handle_info_guild_event_command_huify', [
                            fun handle_info_guild_event_message_create_tokenize/1,
                            fun handle_info_guild_event_message_create_parse/1,
                            fun handle_info_guild_event_command_huify_find_word/1,
                            fun handle_info_guild_event_command_huify_huify/1
                        ], #handle_info_guild_event_command_huify{
                            guild_id = marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event)),
                            allowed_channel_ids = ChannelIds,
                            original_message = OriginalMessage
                        }
                    ) of
                        {ok, ChainCtx} ->
                            marvin_helper_chain:chain(
                                'marvin_plugin_huify:handle_info_guild_event_command_huify_act', [
                                    fun handle_info_guild_event_command_huify_act_send_message/1
                                ], ChainCtx
                            ),
                            {noreply, S0#state{
                                channel_counters = ChannelCounters#{ChannelId := 0}
                            }};
                        {skip, {invalid_word, _ChainCtx}} ->
                            {noreply, S0#state{
                                channel_counters = ChannelCounters#{ChannelId := Count + 1}
                            }}
                    end;
                _ ->
                    ?l_info(#{
                        text => "Plugin skipping the trigger",
                        what => handle_info, result => ok,
                        details => #{
                            guild_id => S0#state.guild_id,
                            probability => Prob
                        }
                    }),
                    {noreply, S0#state{
                        channel_counters = ChannelCounters#{ChannelId := Count + 1}
                    }}
            end;
        #state{channel_counters = #{ChannelId := Count} = ChannelCounters} ->
            ?l_info(#{
                text => "Plugin skipping the trigger (low)",
                what => handle_info, result => ok,
                details => #{
                    guild_id => S0#state.guild_id
                }
            }),
            {noreply, S0#state{
                channel_counters = ChannelCounters#{ChannelId := Count + 1}
            }}
    end.



handle_info_guild_event_message_create_tokenize(#handle_info_guild_event_command_huify{
    original_message = OriginalMessage
} = ChainCtx) ->
    case marvin_guild_command_lexer:string(
        unicode:characters_to_list(marvin_pdu2_dispatch_message_create:content(OriginalMessage))
    ) of
        {ok, Tokens, _} ->
            {ok, ChainCtx#handle_info_guild_event_command_huify{parsed_message_content = Tokens}};
        {error, Reason} ->
            {error, Reason}
    end.



handle_info_guild_event_message_create_parse(#handle_info_guild_event_command_huify{
    parsed_message_content = Tokens
} = ChainCtx) ->
    case marvin_guild_command_parser:parse(Tokens) of
        {ok, ParsedMessage} ->
            {ok, ChainCtx#handle_info_guild_event_command_huify{
                parsed_message_content = ParsedMessage
            }};
        {error, Reason} ->
            {error, Reason}
    end.




handle_info_guild_event_command_huify(Event, S0) ->
    #{<<"channel_ids">> := ChannelIds} = marvin_plugin_config:data(S0#state.config),
    case marvin_helper_chain:chain(
        'marvin_plugin_huify:handle_info_guild_event_command_huify', [
            fun handle_info_guild_event_command_huify_provision_context/1,
            fun handle_info_guild_event_command_huify_ensure_channel_allowed/1,
            fun handle_info_guild_event_command_huify_find_word/1,
            fun handle_info_guild_event_command_huify_huify/1
        ], #handle_info_guild_event_command_huify{
            guild_id = marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event)),
            event = Event,
            allowed_channel_ids = ChannelIds
        }
    ) of
        {ok, ChainCtx} ->
            marvin_helper_chain:chain(
                'marvin_plugin_huify:handle_info_guild_event_command_huify_act', [
                    fun handle_info_guild_event_command_huify_act_send_message/1
                ], ChainCtx
            );
        {skip, {invalid_channel, ChainCtx}} ->
            marvin_helper_chain:chain(
                'marvin_plugin_huify:handle_info_guild_event_command_huify_invalid_channel', [
                    fun handle_info_guild_event_command_huify_act_send_message/1
                ], ChainCtx#handle_info_guild_event_command_huify{
                    message = <<"Это возможно только в тёмной подворотне."/utf8>>
                }
            );
        {skip, {invalid_word, ChainCtx}} ->
            marvin_helper_chain:chain(
                'marvin_plugin_huify:handle_info_guild_event_command_huify_invalid_word', [
                    fun handle_info_guild_event_command_huify_act_send_message/1
                ], ChainCtx#handle_info_guild_event_command_huify{
                    message = <<"Чивооо, блядь?"/utf8>>
                }
            )
    end,
    {noreply, S0}.

handle_info_guild_event_command_huify_provision_context(#handle_info_guild_event_command_huify{
    event = Event
} = ChainCtx) ->
    #{
        original_message := OriginalMessage,
        parsed_message_content := ParsedMessage
    } = marvin_guild_pubsub:payload(Event),
    {ok, ChainCtx#handle_info_guild_event_command_huify{
        original_message = OriginalMessage,
        parsed_message_content = ParsedMessage
    }}.

handle_info_guild_event_command_huify_ensure_channel_allowed(#handle_info_guild_event_command_huify{
    allowed_channel_ids = ChannelIds,
    original_message = OriginalMessage
} = ChainCtx) ->
    case lists:member(marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage), ChannelIds) of
        true -> {ok, ChainCtx};
        false -> {skip, {invalid_channel, ChainCtx}}
    end.

handle_info_guild_event_command_huify_find_word(#handle_info_guild_event_command_huify{
    parsed_message_content = ParsedMessage
} = ChainCtx) ->
    case lists:reverse(
        [BinPart || BinPart <- ParsedMessage,
            is_binary(BinPart),
            <<"кхм"/utf8>> /= BinPart
        ]
    ) of
        [Word | _] -> {ok, ChainCtx#handle_info_guild_event_command_huify{word = Word}};
        _ -> {skip, {invalid_word, ChainCtx}}
    end.

handle_info_guild_event_command_huify_huify(#handle_info_guild_event_command_huify{
    word = Word
} = ChainCtx) ->
    case marvin_plugin_huify_transform:huify(string:lowercase(Word)) of
        {ok, HuifiedWord} ->
            {ok, ChainCtx#handle_info_guild_event_command_huify{
                message = <<HuifiedWord/binary, ", блядь."/utf8>>
            }};
        {error, _} -> {skip, {invalid_word, ChainCtx}}
    end.

handle_info_guild_event_command_huify_act_send_message(#handle_info_guild_event_command_huify{
    guild_id = GuildId,
    original_message = OriginalMessage,
    message = Message
} = ChainCtx) ->
    Req = marvin_rest2_request:new(
        marvin_rest2_impl_message_create,
        #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
        #{
            content => Message,
            message_reference => #{
            message_id => marvin_pdu2_dispatch_message_create:id(OriginalMessage),
            channel_id => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
            guild_id => GuildId
        },
            allowed_mentions => #{replied_user => true}
        }
    ),
    _ = marvin_rest2:enqueue_request(Req),
    {ok, ChainCtx}.
