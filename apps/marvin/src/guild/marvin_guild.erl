-module(marvin_guild).
-behaviour(gen_server).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_context/1,
    do_provision/2, do_provision_guild_members/2,
    member_update/2, presence_update/2, voice_state_update/2, message_create/2,
    channel_create/2, channel_update/2, channel_delete/2,
    start_link/2, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(get_context(), {get_context}).
-define(do_provision(Struct), {do_provision, Struct}).
-define(do_provision_guild_members(Struct), {do_provision_guild_members, Struct}).
-define(member_update(Struct), {member_update, Struct}).
-define(presence_update(Struct), {presence_update, Struct}).
-define(voice_state_update(Struct), {voice_state_update, Struct}).
-define(message_create(Struct), {message_create, Struct}).
-define(channel_create(Struct), {channel_create, Struct}).
-define(channel_update(Struct), {channel_update, Struct}).
-define(channel_delete(Struct), {channel_delete, Struct}).



%% Interface



-spec start_link(GuildId :: marvin_pdu2:snowflake(), MyId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId, MyId) ->
    gen_server:start_link(?MODULE, [GuildId, MyId], []).



-spec get_context(GuildId :: marvin_pdu2:snowflake()) ->
    Ret :: marvin_helper_type:ok_return(marvin_guild_context:t()).

get_context(GuildId) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?get_context()).



-spec do_provision(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_guild_create:t()) ->
    Ret :: marvin_helper_type:ok_return().

do_provision(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?do_provision(Struct)).



-spec member_update(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_member_update:t()) ->
    Ret :: marvin_helper_type:ok_return().

member_update(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?member_update(Struct)).



-spec do_provision_guild_members(
    GuildId :: marvin_pdu2:snowflake(),
    Struct :: marvin_pdu2_dispatch_guild_members_chunk:t()
) ->
    Ret :: marvin_helper_type:ok_return().

do_provision_guild_members(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?do_provision_guild_members(Struct)).



-spec presence_update(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_presence_update:t()) ->
    Ret :: marvin_helper_type:ok_return().

presence_update(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?presence_update(Struct)).



-spec voice_state_update(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_voice_state_update:t()) ->
    Ret :: marvin_helper_type:ok_return().

voice_state_update(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?voice_state_update(Struct)).



-spec message_create(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_message_create:t()) ->
    Ret :: marvin_helper_type:ok_return().

message_create(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?message_create(Struct)).



-spec channel_create(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_channel_create:t()) ->
    Ret :: marvin_helper_type:ok_return().

channel_create(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?channel_create(Struct)).



-spec channel_update(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_channel_update:t()) ->
    Ret :: marvin_helper_type:ok_return().

channel_update(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?channel_update(Struct)).



-spec channel_delete(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_channel_delete:t()) ->
    Ret :: marvin_helper_type:ok_return().

channel_delete(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?channel_delete(Struct)).



init([GuildId, MyId]) ->
    {ok, GuildConfig} = marvin_guild_config:load(GuildId),
    PluginsList = marvin_guild_config:enabled_plugins(GuildConfig),
    Plugins = maybe_start_plugins(GuildId, PluginsList),
    Commands = collect_commands(PluginsList),
    {ok, #state{
        my_id = MyId,
        guild_id = GuildId,
        role_admin_id = marvin_guild_config:role_admin(GuildConfig),
        role_moderator_id = marvin_guild_config:role_moderator(GuildConfig),
        guild_config = GuildConfig,
        plugins = Plugins,
        commands = Commands,
        presence_state = ets:new(marvin_guild_presence_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        role_state = ets:new(marvin_guild_role_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        emoji_state = ets:new(marvin_guild_emoji_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        channel_text_state = ets:new(marvin_guild_channel_text_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        channel_voice_state = ets:new(marvin_guild_channel_voice_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        channel_category_state = ets:new(marvin_guild_channel_category_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        member_state = ets:new(marvin_guild_member_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        voice_state = ets:new(marvin_guild_voice_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ])
    }}.



%% Internals



maybe_start_plugins(GuildId, PluginsList) ->
    maps:from_list(lists:map(fun(PluginId) ->
        case marvin_plugin_sup:start_plugin(binary_to_atom(PluginId, latin1), GuildId) of
            {ok, Pid} ->
                ?l_info(#{
                    text => "Guild plugin start",
                    what => guild_plugin_start, result => ok,
                    details => #{guild_id => GuildId, plugin_id => PluginId, plugin_pid => Pid}
                }),
                {PluginId, Pid};
            {error, {already_started, Pid}} ->
                ?l_notice(#{
                    text => "Guild plugin start",
                    what => guild_plugin_start, result => already_started,
                    details => #{guild_id => GuildId, plugin_id => PluginId, plugin_pid => Pid}
                }),
                {PluginId, Pid};
            {error, Reason} ->
                ?l_error(#{
                    text => "Guild plugin start failed",
                    what => guild_plugin_start, result => failure,
                    details => #{guild_id => GuildId, plugin_id => PluginId, reason => Reason}
                }),
                error(Reason)
        end
    end, PluginsList)).



collect_commands(PluginsList) ->
    lists:flatten(lists:map(fun(PluginId) ->
        (binary_to_atom(PluginId, latin1)):get_commands(any)
    end, PluginsList)).



context_from_state(#state{
    name = Name,
    my_id = MyId,
    guild_id = GuildId,
    owner_id = OwnerId,
    role_admin_id = RoleAdminId,
    role_moderator_id = RoleModeratorId,
    commands = Commands,
    presence_state = PresenceState,
    role_state = RoleState,
    emoji_state = EmojiState,
    channel_text_state = ChannelTextState,
    channel_voice_state = ChannelVoiceState,
    channel_category_state = ChannelCategoryState,
    member_state = MemberState,
    voice_state = VoiceState
}) ->
    marvin_guild_context:new(#{
        name => Name,
        my_id => MyId,
        guild_id => GuildId,
        owner_id => OwnerId,
        role_admin => RoleAdminId,
        role_moderator => RoleModeratorId,
        commands => Commands,
        presence_state => PresenceState,
        role_state => RoleState,
        emoji_state => EmojiState,
        channel_text_state => ChannelTextState,
        channel_voice_state => ChannelVoiceState,
        channel_category_state => ChannelCategoryState,
        member_state => MemberState,
        voice_state => VoiceState
    }).



handle_call(?get_context(), _GenReplyTo, S0) ->
    {reply, {ok, context_from_state(S0)}, S0};

handle_call(?do_provision(Struct), _GenReplyTo, S0) ->
    S1 = S0#state{
        name = marvin_pdu2_dispatch_guild_create:name(Struct),
        owner_id = marvin_pdu2_dispatch_guild_create:owner_id(Struct)
    },
    Ctx = context_from_state(S1),
    ok = marvin_guild_helper_presence:w_do_provision(marvin_pdu2_dispatch_guild_create:presences(Struct), Ctx),
    ok = marvin_guild_helper_role:w_do_provision(marvin_pdu2_dispatch_guild_create:roles(Struct), Ctx),
    ok = marvin_guild_helper_emoji:w_do_provision(marvin_pdu2_dispatch_guild_create:emojis(Struct), Ctx),
    ok = marvin_guild_helper_channel_text:w_do_provision(marvin_pdu2_dispatch_guild_create:channels(Struct), Ctx),
    ok = marvin_guild_helper_channel_voice:w_do_provision(marvin_pdu2_dispatch_guild_create:channels(Struct), Ctx),
    ok = marvin_guild_helper_channel_category:w_do_provision(marvin_pdu2_dispatch_guild_create:channels(Struct), Ctx),
    ok = marvin_guild_helper_voice_state:w_do_provision(marvin_pdu2_dispatch_guild_create:voice_states(Struct), Ctx),
    ?l_info(#{
        text => "Guild provisioning",
        what => handle_call_do_provision_guild, result => ok,
        details => #{
            guild_id => S0#state.guild_id,
            presences_count => length(marvin_pdu2_dispatch_guild_create:presences(Struct)),
            roles_count => length(marvin_pdu2_dispatch_guild_create:roles(Struct)),
            emojis_count => length(marvin_pdu2_dispatch_guild_create:emojis(Struct)),
            channels_text_count => length(marvin_pdu2_dispatch_guild_create:channels(Struct)),
            channels_voice_count => length(marvin_pdu2_dispatch_guild_create:channels(Struct)),
            channels_category_count => length(marvin_pdu2_dispatch_guild_create:channels(Struct)),
            voice_states_count => length(marvin_pdu2_dispatch_guild_create:voice_states(Struct))
        }
    }),
    {reply, ok, S1};

handle_call(?do_provision_guild_members(Struct), _GenReplyTo, S0) ->
    ok = marvin_guild_helper_members:w_do_provision(marvin_pdu2_dispatch_guild_members_chunk:members(Struct), context_from_state(S0)),
    ?l_info(#{
        text => "Guild provisioning (members)",
        what => handle_call_do_provision_guild_members, result => ok,
        details => #{
            guild_id => S0#state.guild_id,
            members_count => length(marvin_pdu2_dispatch_guild_members_chunk:members(Struct))
        }
    }),
    {reply, ok, S0};

handle_call(?member_update(Struct), _GenReplyTo, S0) ->
    ok = marvin_guild_helper_members:w_member_update(Struct, context_from_state(S0)),
    ?l_debug(#{
        text => "Guild member update",
        what => handle_call_member_update, result => ok,
        details => #{
            guild_id => S0#state.guild_id,
            user_id => marvin_pdu2_object_user:id(marvin_pdu2_dispatch_guild_member_update:user(Struct))
        }
    }),
    {reply, ok, S0};

handle_call(?presence_update(Struct), _GenReplyTo, S0) ->
    ok = marvin_guild_helper_presence:w_presence_update(Struct, context_from_state(S0)),
    ?l_debug(#{
        text => "Guild presence update",
        what => handle_call_presence_update, result => ok,
        details => #{
            guild_id => S0#state.guild_id,
            user_id => marvin_pdu2_dispatch_presence_update:user(Struct),
            status => marvin_pdu2_dispatch_presence_update:status(Struct)
        }
    }),
    {reply, ok, S0};

handle_call(?voice_state_update(Struct), _GenReplyTo, S0) ->
    ok = marvin_guild_helper_voice_state:w_voice_state_update(Struct, context_from_state(S0)),
    ?l_debug(#{
        text => "Guild voice state update",
        what => handle_call_voice_state_update, result => ok,
        details => #{
            guild_id => S0#state.guild_id,
            user_id => marvin_pdu2_dispatch_voice_state_update:user_id(Struct),
            channel_id => marvin_pdu2_dispatch_voice_state_update:channel_id(Struct)
        }
    }),
    {reply, ok, S0};

handle_call(?message_create(Struct), _GenReplyTo, S0) ->
    ok = marvin_guild_helper_message:w_message_create(Struct, context_from_state(S0)),
    ?l_debug(#{
        text => "Guild message create",
        what => handle_call_message_create, result => ok,
        details => #{
            guild_id => S0#state.guild_id,
            channel_id => marvin_pdu2_dispatch_message_create:channel_id(Struct),
            user_id => marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(Struct))
        }
    }),
    {reply, ok, S0};

handle_call(?channel_create(Struct), _GenReplyTo, S0) ->
    Ctx = context_from_state(S0),
    ok = marvin_guild_helper_channel_text:w_channel_create(Struct, Ctx),
    ok = marvin_guild_helper_channel_voice:w_channel_create(Struct, Ctx),
    ok = marvin_guild_helper_channel_category:w_channel_create(Struct, Ctx),
    ?l_debug(#{
        text => "Guild channel create",
        what => handle_call_channel_create, result => ok,
        details => #{guild_id => S0#state.guild_id, channel_id => marvin_pdu2_dispatch_channel_create:id(Struct)}
    }),
    {reply, ok, S0};

handle_call(?channel_update(Struct), _GenReplyTo, S0) ->
    Ctx = context_from_state(S0),
    ok = marvin_guild_helper_channel_text:w_channel_update(Struct, Ctx),
    ok = marvin_guild_helper_channel_voice:w_channel_update(Struct, Ctx),
    ok = marvin_guild_helper_channel_category:w_channel_update(Struct, Ctx),
    ?l_debug(#{
        text => "Guild channel update",
        what => handle_call_channel_update, result => ok,
        details => #{guild_id => S0#state.guild_id, channel_id => marvin_pdu2_dispatch_channel_update:id(Struct)}
    }),
    {reply, ok, S0};

handle_call(?channel_delete(Struct), _GenReplyTo, S0) ->
    Ctx = context_from_state(S0),
    ok = marvin_guild_helper_channel_text:w_channel_delete(Struct, Ctx),
    ok = marvin_guild_helper_channel_voice:w_channel_delete(Struct, Ctx),
    ok = marvin_guild_helper_channel_category:w_channel_delete(Struct, Ctx),
    ?l_debug(#{
        text => "Guild channel delete",
        what => handle_call_channel_delete, result => ok,
        details => #{guild_id => S0#state.guild_id, channel_id => marvin_pdu2_dispatch_channel_delete:id(Struct)}
    }),
    {reply, ok, S0};

handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected info", what => handle_info, details => Unexpected}),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.
