-module(marvin_guild_helper_channel_category).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([
    w_do_provision/2, w_channel_create/2,
    w_channel_update/2, w_channel_delete/2
]).



%% Interface



-spec w_do_provision(Channels :: [marvin_pdu2_object_channel:t()], Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_do_provision(Channels, Ctx) ->
    Categories = [
        Channel || Channel <- Channels,
        marvin_pdu2_object_channel:channel_type_guild_category() == marvin_pdu2_object_channel:type(Channel)
    ],
    ?l_info(#{
        text => "Guild categories provisioned",
        what => channel_provision, result => ok,
        details => #{
            guild_id => marvin_guild_context:guild_id(Ctx),
            total => length(Categories),
            channel_type => category
        }
    }),
    lists:foreach(fun(Channel) -> set_channel_category(Channel, Ctx) end, Categories),
    ok.



-spec w_channel_create(ChannelEvent :: marvin_pdu2_dispatch_channel_create:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_create(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_create:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_create:channel_type_guild_category()
    } of
        {_Same, _Same} ->
            ?l_info(#{
                text => "Guild got new category",
                what => channel_create, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    channel_id => marvin_pdu2_dispatch_channel_create:id(ChannelEvent),
                    channel_type => category
                }
            }),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_create:export(ChannelEvent)),
            set_channel_category(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec w_channel_update(ChannelEvent :: marvin_pdu2_dispatch_channel_update:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_update(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_update:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_update:channel_type_guild_category()
    } of
        {_Same, _Same} ->
            ?l_info(#{
                text => "Guild got updated category",
                what => channel_update, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    channel_id => marvin_pdu2_dispatch_channel_update:id(ChannelEvent),
                    channel_type => category
                }
            }),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_update:export(ChannelEvent)),
            set_channel_category(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



-spec w_channel_delete(ChannelEvent :: marvin_pdu2_dispatch_channel_delete:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

w_channel_delete(ChannelEvent, Ctx) ->
    case {
        marvin_pdu2_dispatch_channel_delete:type(ChannelEvent),
        marvin_pdu2_dispatch_channel_delete:channel_type_guild_category()
    } of
        {_Same, _Same} ->
            ?l_info(#{
                text => "Guild lost category",
                what => channel_delete, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    channel_id => marvin_pdu2_dispatch_channel_delete:id(ChannelEvent),
                    channel_type => category
                }
            }),
            Channel = marvin_pdu2_object_channel:new(marvin_pdu2_dispatch_channel_delete:export(ChannelEvent)),
            delete_channel_category(Channel, Ctx),
            ok;
        {_, _} ->
            ok
    end.



%% Internals



-spec set_channel_category(Channel :: marvin_pdu2_object_channel:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

set_channel_category(Channel, Ctx) ->
    ChannelId = marvin_pdu2_object_channel:id(Channel),
    marvin_channel_registry:register(marvin_guild_context:guild_id(Ctx), ChannelId),
    ets:insert(marvin_guild_context:channel_category_state(Ctx), #channel{channel_id = ChannelId, channel = Channel}),
    ok.



-spec delete_channel_category(Channel :: marvin_pdu2_object_channel:t(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_helper_type:ok_return().

delete_channel_category(Channel, Ctx) ->
    ChannelId = marvin_pdu2_object_channel:id(Channel),
    marvin_channel_registry:register(marvin_guild_context:guild_id(Ctx), ChannelId),
    ets:delete(marvin_guild_context:channel_category_state(Ctx), ChannelId),
    ok.
