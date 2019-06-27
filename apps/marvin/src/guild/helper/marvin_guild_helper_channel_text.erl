-module(marvin_guild_helper_channel_text).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([
    w_do_provision/2, w_channel_create/2,
    w_channel_update/2, w_channel_delete/2,
    r_get_channel_by_id/2
]).



%% Interface



-spec w_do_provision(Channels :: [marvin_pdu2_object_channel:t()], Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_do_provision(Channels, Ctx) ->
    TextChannels = [
        Channel || Channel <- Channels,
        marvin_pdu2_object_channel:channel_type_guild_text() == marvin_pdu2_object_channel:type(Channel)
    ],
    ?l_debug(#{
        text => "Guild text channels provisioned",
        what => channel_provision, result => ok,
        details => #{
            guild_id => marvin_guild_context:guild_id(Ctx),
            total => length(TextChannels),
            channel_type => text
        }
    }),
    lists:foreach(fun(Channel) -> set_channel_text(Channel, Ctx) end, TextChannels),
    ok.



-spec w_channel_create(ChannelEvent :: marvin_pdu2_dispatch_channel_create:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_create(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_create:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_create:channel_type_guild_text()
    } of
        {_Same, _Same} ->
            ?l_debug(#{
                text => "Guild got new text channel",
                what => channel_create, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    channel_id => marvin_pdu2_dispatch_channel_create:id(ChannelEvent),
                    channel_type => text
                }
            }),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_create:export(ChannelEvent)),
            set_channel_text(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec w_channel_update(ChannelEvent :: marvin_pdu2_dispatch_channel_update:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_update(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_update:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_update:channel_type_guild_text()
    } of
        {_Same, _Same} ->
            ?l_debug(#{
                text => "Guild got updated text channel",
                what => channel_update, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    channel_id => marvin_pdu2_dispatch_channel_update:id(ChannelEvent),
                    channel_type => text
                }
            }),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_update:export(ChannelEvent)),
            set_channel_text(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec w_channel_delete(ChannelEvent :: marvin_pdu2_dispatch_channel_delete:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_delete(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_delete:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_delete:channel_type_guild_text()
    } of
        {_Same, _Same} ->
            ?l_debug(#{
                text => "Guild lost text channel",
                what => channel_delete, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    channel_id => marvin_pdu2_dispatch_channel_delete:id(ChannelEvent),
                    channel_type => text
                }
            }),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_delete:export(ChannelEvent)),
            delete_channel_text(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec r_get_channel_by_id(ChannelId :: marvin_pdu2:snowflake(), Ctx :: marvin_guild_context:t()) ->
    marvin_pdu2_object_role:t() | undefined.

r_get_channel_by_id(ChannelId, Ctx) ->
    case ets:lookup(marvin_guild_context:channel_text_state(Ctx), ChannelId) of
        [#channel{channel = Channel}] ->
            Channel;
        [] ->
            undefined
    end.



%% Internals



-spec set_channel_text(Channel :: marvin_pdu2_object_channel:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

set_channel_text(Channel, Ctx) ->
    ChannelId = marvin_pdu2_object_channel:id(Channel),
    marvin_channel_registry:register(marvin_guild_context:guild_id(Ctx), ChannelId),
    ets:insert(marvin_guild_context:channel_text_state(Ctx), #channel{channel_id = ChannelId, channel = Channel}),
    ok.



-spec delete_channel_text(Channel :: marvin_pdu2_object_channel:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

delete_channel_text(Channel, Ctx) ->
    ChannelId = marvin_pdu2_object_channel:id(Channel),
    marvin_channel_registry:register(marvin_guild_context:guild_id(Ctx), ChannelId),
    ets:delete(marvin_guild_context:channel_text_state(Ctx), ChannelId),
    ok.
