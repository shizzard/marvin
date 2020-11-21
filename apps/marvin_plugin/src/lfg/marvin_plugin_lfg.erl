-module(marvin_plugin_lfg).
-behaviour(gen_server).

-include("marvin_plugin_lfg.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").
-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    is_game_role_name/2,
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([get_pre_command_hook_fun/2]).

-define(init_pre_command_hook(), {init_pre_command_hook}).



%% Interface



is_game_role_name(Prefix, Role) ->
    PrefixLen = erlang:byte_size(Prefix),
    case Role of
        <<Prefix:PrefixLen/binary, _Rest/binary>> -> true;
        _ -> false
    end.



-spec get_commands(L10n :: binary()) ->
    Ret :: [marvin_plugin_command:t()].

get_commands(_) ->
    [].



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    timer:send_after(5000, ?init_pre_command_hook()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_internal(), marvin_guild_pubsub:action_internal_event_guild_provisioned()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_internal(), marvin_guild_pubsub:action_internal_event_guild_members_provisioned()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_member(), marvin_guild_pubsub:action_update()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_presence(), marvin_guild_pubsub:action_update()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_role(), marvin_guild_pubsub:action_create()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_role(), marvin_guild_pubsub:action_update()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_role(), marvin_guild_pubsub:action_delete()),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_message(), marvin_guild_pubsub:action_create()),
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId
    }}.



%% Internals



get_pre_command_hook_fun(RoleGamePrefix, ChannelId) ->
    fun(Event) ->
        OriginalMessage = marvin_guild_pubsub:payload(Event),
        case marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage) of
            ChannelId ->
                skip;
            _ ->
                GuildCtx = marvin_guild_pubsub:guild_context(Event),
                OriginalMessage,
                case lists:any(fun(RoleId) ->
                    Role = marvin_guild_helper_role:r_get_role_by_id(RoleId, GuildCtx),
                    marvin_plugin_lfg:is_game_role_name(RoleGamePrefix, marvin_pdu2_object_role:name(Role))
                end, marvin_pdu2_dispatch_message_create:mention_roles(OriginalMessage)) of
                    true ->
                        TargetChannel = marvin_guild_helper_channel_text:r_get_channel_by_id(ChannelId, GuildCtx),
                        _ = marvin_rest2:enqueue_request(marvin_rest2_request:new(
                            marvin_rest2_impl_message_delete,
                            #{
                                <<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
                                <<"message_id">> => marvin_pdu2_dispatch_message_create:id(OriginalMessage)
                            },
                            #{}
                        )),
                        _ = marvin_rest2:enqueue_request(marvin_rest2_request:new(
                            marvin_rest2_impl_message_create,
                            #{<<"channel_id">> => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage)},
                            #{
                                content => iolist_to_binary([
                                    <<"Слыш ">>,
                                    marvin_pdu2_object_user:format(marvin_pdu2_dispatch_message_create:author(OriginalMessage)),
                                    <<" тереби в "/utf8>>, marvin_pdu2_object_channel:format(TargetChannel), <<".">>
                                ]),
                                message_reference => #{
                                    message_id => marvin_pdu2_dispatch_message_create:id(OriginalMessage),
                                    channel_id => marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
                                    guild_id => marvin_guild_context:guild_id(GuildCtx)
                                },
                                allowed_mentions => #{replied_user => true}
                            }
                        )),
                        terminate;
                    false ->
                        skip
                end
        end
    end.



handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(?init_pre_command_hook(), #state{
    config = PluginConfig,
    guild_id = GuildId
} = S0) ->
    ?l_info(#{
        text => "Plugin is installing pre_command_hook",
        what => handle_info,
        details => #{
            guild_id => S0#state.guild_id,
            plugin_id => ?MODULE
        }
    }),
    #{
        <<"role_game_prefix">> := RoleGamePrefix,
        <<"channel_id">> := ChannelId
    } = marvin_plugin_config:data(PluginConfig),
    PreCommandHookFun = get_pre_command_hook_fun(RoleGamePrefix, ChannelId),
    ok = marvin_guild:set_pre_command_hook(GuildId, ?MODULE, PreCommandHookFun),
    {noreply, S0};

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
    TypeMember = marvin_guild_pubsub:type_member(),
    TypePresence = marvin_guild_pubsub:type_presence(),
    TypeRole = marvin_guild_pubsub:type_role(),
    TypeInternal = marvin_guild_pubsub:type_internal(),
    ActionCreate = marvin_guild_pubsub:action_create(),
    ActionUpdate = marvin_guild_pubsub:action_update(),
    ActionIEGP = marvin_guild_pubsub:action_internal_event_guild_provisioned(),
    ActionIEGMP = marvin_guild_pubsub:action_internal_event_guild_members_provisioned(),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeInternal, ActionIEGP} ->
            marvin_plugin_lfg_handler_guild_provisioned:handle(Event, S0);
        {TypeInternal, ActionIEGMP} ->
            marvin_plugin_lfg_handler_guild_members_provisioned:handle(Event, S0);
        {TypeMessage, ActionCreate} ->
            marvin_plugin_lfg_handler_message_create:handle_message_create(Event, S0);
        {TypeMember, ActionUpdate} ->
            marvin_plugin_lfg_handler_member_update:handle_member_update(Event, S0);
        {TypePresence, ActionUpdate} ->
            marvin_plugin_lfg_handler_member_update:handle_presence_update(Event, S0);
        {TypeRole, _AnyAction} ->
            marvin_plugin_lfg_handler_role_change:handle(Event, S0);
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
