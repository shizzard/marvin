-module(marvin_plugin_voice_on_demand).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(active_channel, {
    channel_name :: unicode:unicode_binary(),
    origin_channel_id = undefined :: marvin_pdu2:snowflake() | undefined,
    owner = undefined :: marvin_pdu2_object_user:t() | undefined,
    channel_id :: marvin_pdu2:snowflake() | undefined,
    created_at = os:timestamp() :: erlang:timestamp(),
    used = false :: boolean()
}).

-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake(),
    active_channels :: ets:tid(),
    cleanup_timer :: timer:tref(),
    adjectives :: [binary()],
    nouns :: [binary()]
}).
-type state() :: #state{}.

-define(message_template_placeholder_channel_name, <<"{{channel_name}}">>).

-define(noun_terms, "/voice_on_demand/noun.terms").
-define(adjective_terms, "/voice_on_demand/adjective.terms").
-define(participats_limit_min, 1).
-define(participats_limit_max, 99).
-define(max_channel_name_gen, 15).
-define(cleanup_event(), {cleanup_event}).
-define(cleanup_interval, 20).
-define(channel_immune_period, 30).
-define(do_rescan_category(), {do_rescan_category}).



%% Interface



-spec get_commands(L10n :: binary()) ->
    Ret :: [marvin_plugin_command:t()].

get_commands(_) ->
    [command_create()].



command_create() ->
    marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_voice_on_demand">>,
        command => <<"create_channel">>,
        help => <<
            "Создает временный голосовой канал, который будет удален после использования.\n"/utf8,
            "Если в команде будут упомянуты конкретные пользователи или группы пользователей, доступ в канал будет только у автора команды и упомянутых людей.\n"/utf8,
            "В команде можно указать вместимость канала от 1 до 99."/utf8
        >>,
        keywords => [<<"войс"/utf8>>, <<"канал"/utf8>>, <<"голос"/utf8>>, <<"комната"/utf8>>]
    }).



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(command_create())),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_channel_voice(), marvin_guild_pubsub:action_create()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_channel_voice(), marvin_guild_pubsub:action_delete()),
    {ok, Adjectives} = file:consult(code:priv_dir(marvin_plugin) ++ ?adjective_terms),
    {ok, Nouns} = file:consult(code:priv_dir(marvin_plugin) ++ ?noun_terms),
    erlang:send_after(5000, self(), ?do_rescan_category()),
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId,
        active_channels = ets:new(marvin_plugin_voice_on_demand_active_channels, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        cleanup_timer = init_cleanup_timer(),
        adjectives = Adjectives,
        nouns = Nouns
    }}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(?do_rescan_category(), S0) ->
    ok = scan_category(S0),
    {noreply, S0};

handle_info(?cleanup_event(), S0) ->
    handle_info_cleanup_event(S0);

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



scan_category(S0) ->
    {ok, GuildCtx} = marvin_guild:get_context(S0#state.guild_id),
    #{<<"category_id">> := CategoryId} = marvin_plugin_config:data(S0#state.config),
    [
        insert_channel(S0#state.active_channels, #active_channel{
            channel_name = marvin_pdu2_object_channel:name(Channel),
            channel_id = marvin_pdu2_object_channel:id(Channel)
        }) ||
        Channel <- marvin_guild_helper_channel_voice:r_get_channels_by_category(CategoryId, GuildCtx)
    ],
    ok.



init_cleanup_timer() ->
    erlang:send_after(?cleanup_interval * 1000, self(), ?cleanup_event()).



handle_info_cleanup_event(S0) ->
    {ok, GuildCtx} = marvin_guild:get_context(S0#state.guild_id),
    ChannelsToCheck = lists:filter(fun(ActiveChannel) ->
        timer:now_diff(os:timestamp(), ActiveChannel#active_channel.created_at) > (?channel_immune_period * 1000 * 1000)
    end, ets:tab2list(S0#state.active_channels)),
    ChannelsToDelete = lists:filter(fun(ActiveChannel) ->
        length(marvin_guild_helper_voice_state:r_get_channel_participants(ActiveChannel#active_channel.channel_id, GuildCtx)) == 0
    end, ChannelsToCheck),
    S1 = lists:foldl(fun handle_info_cleanup_event_channel_delete/2, S0, ChannelsToDelete),
    {noreply, S1#state{cleanup_timer = init_cleanup_timer()}}.



handle_info_cleanup_event_channel_delete(ActiveChannel, S0) ->
    ?l_info(#{
        text => "Plugin is deleting voice channel",
        what => handle_info,
        details => #{
            guild_id => S0#state.guild_id,
            channel_id => ActiveChannel#active_channel.channel_id,
            channel_name => ActiveChannel#active_channel.channel_name
        }
    }),
    Req = marvin_rest2_request:new(
        marvin_rest2_impl_guild_channel_delete,
        #{<<"channel_id">> => ActiveChannel#active_channel.channel_id},
        #{}
    ),
    _ = marvin_rest2:enqueue_request(Req),
    S0.



handle_info_guild_event(Event, S0) ->
    TypeCommand = marvin_guild_pubsub:type_command(),
    TypeChannelVoice = marvin_guild_pubsub:type_channel_voice(),
    ShortCommandCreate = marvin_plugin_command:short(command_create()),
    ActionCreate = marvin_guild_pubsub:action_create(),
    ActionDelete = marvin_guild_pubsub:action_delete(),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeCommand, ShortCommandCreate} ->
            handle_info_guild_event_command_create(Event, S0);
        {TypeChannelVoice, ActionCreate} ->
            handle_info_guild_event_channel_voice_create(Event, S0);
        {TypeChannelVoice, ActionDelete} ->
            handle_info_guild_event_channel_voice_delete(Event, S0);
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



handle_info_guild_event_command_create(Event, S0) ->
    #{<<"category_id">> := CategoryId} = marvin_plugin_config:data(S0#state.config),
    #{
        original_message := OriginalMessage,
        parsed_message_content := ParsedMessage
    } = marvin_guild_pubsub:payload(Event),
    ChannelName = generate_channel_name(S0#state.adjectives, S0#state.nouns, S0#state.active_channels),
    ?l_info(#{
        text => "Plugin is creating voice channel",
        what => handle_info,
        details => #{
            guild_id => S0#state.guild_id,
            channel_name => ChannelName
        }
    }),
    UserLimit = maybe_get_user_limit(ParsedMessage),
    Req = marvin_rest2_request:new(
        marvin_rest2_impl_guild_channel_create,
        #{<<"guild_id">> => marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event))},
        #{
            type => marvin_pdu2_rest_guild_channel_create:channel_type_guild_voice(),
            name => ChannelName,
            bitrate => 64000,
            parent_id => CategoryId,
            user_limit => UserLimit,
            permission_overwrites => maybe_get_permission_overwrites(
                OriginalMessage,
                marvin_guild_pubsub:guild_context(Event)
            )
        }
    ),
    _ = marvin_rest2:enqueue_request(Req),
    insert_channel(S0#state.active_channels, #active_channel{
        channel_name = ChannelName,
        origin_channel_id = marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
        owner = marvin_pdu2_dispatch_message_create:author(OriginalMessage)
    }),
    {noreply, S0}.



handle_info_guild_event_channel_voice_create(Event, S0) ->
    OriginalEvent = marvin_guild_pubsub:payload(Event),
    ChannelName = marvin_pdu2_dispatch_channel_create:name(OriginalEvent),
    case lookup_channel(S0#state.active_channels, ChannelName) of
        {ok, ActiveChannel} ->
            ?l_info(#{
                text => "Plugin voice channel created",
                what => handle_info,
                details => #{
                    guild_id => S0#state.guild_id,
                    channel_id => ActiveChannel#active_channel.channel_id,
                    channel_name => ActiveChannel#active_channel.channel_name
                }
            }),
            insert_channel(S0#state.active_channels, ActiveChannel#active_channel{
                channel_id = marvin_pdu2_dispatch_channel_create:id(OriginalEvent)
            }),
            Req = marvin_rest2_request:new(
                marvin_rest2_impl_message_create,
                #{<<"channel_id">> => ActiveChannel#active_channel.origin_channel_id},
                #{content => <<"Канал '"/utf8, (ActiveChannel#active_channel.channel_name)/binary, "' готов."/utf8>>}
            ),
            _ = marvin_rest2:enqueue_request(Req),
            {noreply, S0};
        {error, not_found} ->
            {noreply, S0}
    end.



handle_info_guild_event_channel_voice_delete(Event, S0) ->
    OriginalEvent = marvin_guild_pubsub:payload(Event),
    ChannelName = marvin_pdu2_dispatch_channel_delete:name(OriginalEvent),
    case lookup_channel(S0#state.active_channels, ChannelName) of
        {ok, ActiveChannel} ->
            ?l_info(#{
                text => "Plugin voice channel deleted",
                what => handle_info,
                details => #{
                    guild_id => S0#state.guild_id,
                    channel_id => ActiveChannel#active_channel.channel_id,
                    channel_name => ActiveChannel#active_channel.channel_name
                }
            }),
            delete_channel(S0#state.active_channels, ChannelName),
            {noreply, S0};
        {error, not_found} ->
            {noreply, S0}
    end.



generate_channel_name(Adjectives, Nouns, Ets) ->
    generate_channel_name(Adjectives, Nouns, Ets, 0).



maybe_get_user_limit(ParsedMessage) ->
    case [Integer || Integer <- ParsedMessage, is_integer(Integer)] of
        [] -> undefined;
        List -> case hd(List) of
            Number when Number =< ?participats_limit_min -> undefined;
            Number when Number >= ?participats_limit_max -> ?participats_limit_max;
            Number -> Number
        end
    end.



generate_channel_name(_Adjectives, _Nouns, _Ets, Gen)
when Gen > ?max_channel_name_gen ->
    throw({generate_channel_name, max_channel_name_gen_reached});

generate_channel_name(Adjectives, Nouns, Ets, Gen) ->
    Adjective = lists:nth(rand:uniform(length(Adjectives)), Adjectives),
    Noun = lists:nth(rand:uniform(length(Nouns)), Nouns),
    ChannelName = <<Adjective/binary, " ", Noun/binary>>,
    case lookup_channel(Ets, ChannelName) of
        {ok, _} ->
            generate_channel_name(Adjectives, Nouns, Ets, Gen + 1);
        {error, not_found} ->
            ChannelName
    end.



maybe_get_permission_overwrites(Message, Ctx) ->
    MyId = marvin_guild_context:my_id(Ctx),
    UserIds = [marvin_pdu2_object_user:id(User) || User <- marvin_pdu2_dispatch_message_create:mentions(Message), marvin_pdu2_object_user:id(User) /= MyId],
    RoleIds = [Role || Role <- marvin_pdu2_dispatch_message_create:mention_roles(Message)],
    case
        maybe_get_permission_overwrites_users(UserIds)
        ++ maybe_get_permission_overwrites_roles(RoleIds)
    of
        [] ->
            %% public channel since no overrides defined
            [];
        SomeOverrides ->
            %% private channel, adding message author and self user and denying everyone
            maybe_get_permission_overwrites_users([
                MyId, marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(Message))
            ]) ++ [#{
                id => marvin_pdu2_object_role:id(marvin_guild_helper_role:r_get_everyone(Ctx)),
                type => marvin_pdu2_object_permission_overwrite:type_role(),
                allow => marvin_pdu2_object_permissions:empty(),
                deny => marvin_pdu2_object_permissions:create([
                    marvin_pdu2_object_permissions:connect(),
                    marvin_pdu2_object_permissions:speak(),
                    marvin_pdu2_object_permissions:use_vad()
                ])
            }] ++ SomeOverrides
    end.



maybe_get_permission_overwrites_users(UserIds) ->
    [#{
        id => UserId,
        type => marvin_pdu2_object_permission_overwrite:type_member(),
        allow => marvin_pdu2_object_permissions:create([
            marvin_pdu2_object_permissions:connect(),
            marvin_pdu2_object_permissions:speak(),
            marvin_pdu2_object_permissions:use_vad()
        ]),
        deny => marvin_pdu2_object_permissions:empty()
    } || UserId <- UserIds].



maybe_get_permission_overwrites_roles(RoleIds) ->
    [#{
        id => RoleId,
        type => marvin_pdu2_object_permission_overwrite:type_role(),
        allow => marvin_pdu2_object_permissions:create([
            marvin_pdu2_object_permissions:connect(),
            marvin_pdu2_object_permissions:speak(),
            marvin_pdu2_object_permissions:use_vad()
        ]),
        deny => marvin_pdu2_object_permissions:empty()
    } || RoleId <- RoleIds].







lookup_channel(Ets, ChannelName) ->
    case ets:lookup(Ets, ChannelName) of
        [Channel] ->
            {ok, Channel};
        [] ->
            {error, not_found}
    end.



insert_channel(Ets, Channel) ->
    ?l_debug(#{
        text => "Plugin voice channel inserted for monitoring",
        what => insert_channel,
        details => #{
            channel => Channel
        }
    }),
    ets:insert(Ets, Channel),
    ok.



delete_channel(Ets, ChannelName) ->
    ?l_debug(#{
        text => "Plugin voice channel deleted from monitoring",
        what => delete_channel,
        details => #{
            channel_name => ChannelName
        }
    }),
    ets:delete(Ets, ChannelName),
    ok.
