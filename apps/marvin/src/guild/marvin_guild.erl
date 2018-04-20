-module(marvin_guild).
-behaviour(gen_server).

-include("marvin_guild_state.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_context/1,
    do_provision/2, do_provision_guild_members/2, presence_update/2, voice_state_update/2, message_create/2,
    channel_create/2, channel_update/2, channel_delete/2,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(get_context(), {get_context}).
-define(do_provision(Struct), {do_provision, Struct}).
-define(do_provision_guild_members(Struct), {do_provision_guild_members, Struct}).
-define(presence_update(Struct), {presence_update, Struct}).
-define(voice_state_update(Struct), {voice_state_update, Struct}).
-define(message_create(Struct), {message_create, Struct}).
-define(channel_create(Struct), {channel_create, Struct}).
-define(channel_update(Struct), {channel_update, Struct}).
-define(channel_delete(Struct), {channel_delete, Struct}).



%% Interface



-spec start_link(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



get_context(GuildId) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?get_context()).



-spec do_provision(GuildId :: marvin_pdu2:snowflake(), Struct :: marvin_pdu2_dispatch_guild_create:t()) ->
    Ret :: marvin_helper_type:ok_return().

do_provision(GuildId, Struct) ->
    {ok, GuildPid} = marvin_guild_monitor:get_guild(GuildId),
    gen_server:call(GuildPid, ?do_provision(Struct)).



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



init([GuildId]) ->
    {ok, GuildConfig} = marvin_guild_config:load(GuildId),
    {ok, #state{
        guild_id = GuildId,
        guild_config = GuildConfig,
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



context_from_state(#state{
    guild_id = GuildId,
    owner_id = OwnerId,
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
        guild_id => GuildId,
        owner_id => OwnerId,
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
    {reply, context_from_state(S0), S0};

handle_call(?do_provision(Struct), _GenReplyTo, S0) ->
    marvin_log:info("Guild '~s' is being provisioned", [S0#state.guild_id]),
    S1 = S0#state{owner_id = marvin_pdu2_dispatch_guild_create:owner_id(Struct)},
    Ctx = context_from_state(S1),
    ok = marvin_guild_helper_presence:w_do_provision(marvin_pdu2_dispatch_guild_create:presences(Struct), Ctx),
    ok = marvin_guild_helper_role:w_do_provision(marvin_pdu2_dispatch_guild_create:roles(Struct), Ctx),
    ok = marvin_guild_helper_emoji:w_do_provision(marvin_pdu2_dispatch_guild_create:emojis(Struct), Ctx),
    ok = marvin_guild_helper_channel_text:w_do_provision(marvin_pdu2_dispatch_guild_create:channels(Struct), Ctx),
    ok = marvin_guild_helper_channel_voice:w_do_provision(marvin_pdu2_dispatch_guild_create:channels(Struct), Ctx),
    ok = marvin_guild_helper_channel_category:w_do_provision(marvin_pdu2_dispatch_guild_create:channels(Struct), Ctx),
    {reply, ok, S1};

handle_call(?do_provision_guild_members(Struct), _GenReplyTo, S0) ->
    marvin_log:info("Guild '~s' is being provisioned with members", [S0#state.guild_id]),
    ok = marvin_guild_helper_members:w_do_provision(marvin_pdu2_dispatch_guild_members_chunk:members(Struct), context_from_state(S0)),
    {reply, ok, S0};

handle_call(?presence_update(Struct), _GenReplyTo, S0) ->
    marvin_log:debug("Guild '~s' got presence update", [S0#state.guild_id]),
    ok = marvin_guild_helper_presence:w_presence_update(Struct, context_from_state(S0)),
    {reply, ok, S0};

handle_call(?voice_state_update(Struct), _GenReplyTo, S0) ->
    marvin_log:debug("Guild '~s' got voice state update", [S0#state.guild_id]),
    ok = marvin_guild_helper_voice_state:w_voice_state_update(Struct, context_from_state(S0)),
    {reply, ok, S0};

handle_call(?message_create(Struct), _GenReplyTo, S0) ->
    marvin_log:debug("Guild '~s' got message", [S0#state.guild_id]),
    ok = marvin_guild_helper_message:w_message_create(Struct, context_from_state(S0)),
    {reply, ok, S0};

handle_call(?channel_create(Struct), _GenReplyTo, S0) ->
    marvin_log:debug("Guild '~s' got channel create event", [S0#state.guild_id]),
    Ctx = context_from_state(S0),
    ok = marvin_guild_helper_channel_text:w_channel_create(Struct, Ctx),
    ok = marvin_guild_helper_channel_voice:w_channel_create(Struct, Ctx),
    ok = marvin_guild_helper_channel_category:w_channel_create(Struct, Ctx),
    {reply, ok, S0};

handle_call(?channel_update(Struct), _GenReplyTo, S0) ->
    marvin_log:debug("Guild '~s' got channel update event", [S0#state.guild_id]),
    Ctx = context_from_state(S0),
    ok = marvin_guild_helper_channel_text:w_channel_update(Struct, Ctx),
    ok = marvin_guild_helper_channel_voice:w_channel_update(Struct, Ctx),
    ok = marvin_guild_helper_channel_category:w_channel_update(Struct, Ctx),
    {reply, ok, S0};

handle_call(?channel_delete(Struct), _GenReplyTo, S0) ->
    marvin_log:debug("Guild '~s' got channel delete event", [S0#state.guild_id]),
    Ctx = context_from_state(S0),
    ok = marvin_guild_helper_channel_text:w_channel_delete(Struct, Ctx),
    ok = marvin_guild_helper_channel_voice:w_channel_delete(Struct, Ctx),
    ok = marvin_guild_helper_channel_category:w_channel_delete(Struct, Ctx),
    {reply, ok, S0};

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.
