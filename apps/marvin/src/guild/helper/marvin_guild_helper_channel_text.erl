-module(marvin_guild_helper_channel_text).

-include("marvin_guild_state.hrl").

-export([
    handle_call_do_provision_chain/1, handle_call_channel_create_chain/1,
    handle_call_channel_update_chain/1, handle_call_channel_delete_chain/1
]).



%% Interface



-spec handle_call_do_provision_chain({Struct :: marvin_pdu2_dispatch_guild_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_guild_create:t(),
        S1 :: state()
    }).

handle_call_do_provision_chain({Struct, S0}) ->
    Channels = [
        Channel || Channel <- marvin_pdu2_dispatch_guild_create:channels(Struct),
        marvin_pdu2_object_channel:channel_type_guild_text() == marvin_pdu2_object_channel:type(Channel)
    ],
    marvin_log:info("Guild '~s' text channels: ~p total", [S0#state.guild_id, length(Channels)]),
    S1 = lists:foldl(fun set_channel_text/2, S0, Channels),
    {ok, {Struct, S1}}.



-spec handle_call_channel_create_chain({Struct :: marvin_pdu2_dispatch_channel_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_channel_create:t(),
        S1 :: state()
    }).

handle_call_channel_create_chain({Struct, S0}) ->
    case {
        marvin_pdu2_dispatch_channel_create:type(Struct),
        marvin_pdu2_dispatch_channel_create:channel_type_guild_text()
    } of
        {_Same, _Same} ->
            marvin_log:info(
                "Guild '~s' got new text channel '~s'",
                [S0#state.guild_id, marvin_pdu2_dispatch_channel_create:id(Struct)
            ]),
            ChannelStruct = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_create:export(Struct)),
            S1 = set_channel_text(ChannelStruct, S0),
            {ok, {Struct, S1}};
        {_, _} ->
            {ok, {Struct, S0}}
    end.



-spec handle_call_channel_update_chain({Struct :: marvin_pdu2_dispatch_channel_update:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_channel_update:t(),
        S1 :: state()
    }).

handle_call_channel_update_chain({Struct, S0}) ->
    case {
        marvin_pdu2_dispatch_channel_update:type(Struct),
        marvin_pdu2_dispatch_channel_update:channel_type_guild_text()
    } of
        {_Same, _Same} ->
            marvin_log:info(
                "Guild '~s' got updated text channel '~s'",
                [S0#state.guild_id, marvin_pdu2_dispatch_channel_update:id(Struct)
            ]),
            ChannelStruct = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_update:export(Struct)),
            S1 = set_channel_text(ChannelStruct, S0),
            {ok, {Struct, S1}};
        {_, _} ->
            {ok, {Struct, S0}}
    end.



-spec handle_call_channel_delete_chain({Struct :: marvin_pdu2_dispatch_channel_delete:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_channel_delete:t(),
        S1 :: state()
    }).

handle_call_channel_delete_chain({Struct, S0}) ->
    case {
        marvin_pdu2_dispatch_channel_delete:type(Struct),
        marvin_pdu2_dispatch_channel_delete:channel_type_guild_text()
    } of
        {_Same, _Same} ->
            marvin_log:info(
                "Guild '~s' lost text channel '~s'",
                [S0#state.guild_id, marvin_pdu2_dispatch_channel_delete:id(Struct)
            ]),
            ChannelId = marvin_pdu2_dispatch_channel_delete:id(Struct),
            S1 = delete_channel_text(ChannelId, S0),
            {ok, {Struct, S1}};
        {_, _} ->
            {ok, {Struct, S0}}
    end.



%% Internals



-spec set_channel_text(ChannelStruct :: marvin_pdu2_object_channel:t(), S0 :: state()) ->
    Ret :: state().

set_channel_text(ChannelStruct, #state{
    channel_text_state = ChannelStateEts,
    guild_id = GuildId
} = S0) ->
    ChannelId = marvin_pdu2_object_channel:id(ChannelStruct),
    marvin_channel_registry:register(GuildId, ChannelId),
    ets:insert(ChannelStateEts, #channel{channel_id = ChannelId, channel = ChannelStruct}),
    S0.



-spec delete_channel_text(ChannelId :: marvin_pdu2:snowflake(), S0 :: state()) ->
    Ret :: state().

delete_channel_text(ChannelId, #state{
    channel_text_state = ChannelStateEts,
    guild_id = GuildId
} = S0) ->
    marvin_channel_registry:unregister(GuildId, ChannelId),
    ets:delete(ChannelStateEts, ChannelId),
    S0.
