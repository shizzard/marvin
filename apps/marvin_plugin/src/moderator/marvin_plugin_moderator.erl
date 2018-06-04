-module(marvin_plugin_moderator).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    get_commands/1,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(handle_info_guild_event_command_mute, {
    role_moderator :: marvin_pdu2:snowflake(),
    role_mute :: marvin_pdu2:snowflake(),
    channel_id :: marvin_pdu2:snowflake(),
    caller :: marvin_pdu2_object_member:t(),
    subjects :: [marvin_pdu2_object_member:t(), ...],
    subjects_muted :: [marvin_pdu2_object_member:t(), ...],
    duration :: pos_integer(),
    event :: map()
}).

-record(active_mute, {
    user_id :: marvin_pdu2:snowflake(),
    mute_till :: non_neg_integer()
}).

-record(state, {
    config :: marvin_plugin_config:t(),
    guild_id :: marvin_pdu2:snowflake(),
    active_mutes :: ets:tid(),
    cleanup_timer :: timer:tref()
}).
-type state() :: #state{}.

-define(message_template_placeholder_, <<"{{}}">>).

-define(cleanup_event(), {cleanup_event}).
-define(cleanup_interval, 20).
-define(default_duration, 10).



%% Interface



-spec get_commands(L10n :: binary()) ->
    Ret :: [marvin_plugin_command:t()].

get_commands(_) ->
    [command_mute()].



command_mute() ->
    marvin_plugin_command:new(#{
        plugin_id => <<"marvin_plugin_moderator">>,
        command => <<"mute">>,
        help => <<
            "Затыкает указанного пользователя или пользователей на указанное количество минут.\n"/utf8,
            "Применимо только модераторами. Время наказания по умолчанию - 10 минут."/utf8
        >>,
        keywords => [<<"мьют"/utf8>>, <<"накажи"/utf8>>, <<"арестуй"/utf8>>, <<"заткни"/utf8>>]
    }).



-spec start_link(GuildId :: marvin_pdu2:snowflake()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



init([GuildId]) ->
    {ok, PluginConfig} = marvin_plugin_config:load(list_to_binary(atom_to_list(?MODULE)), GuildId),
    marvin_guild_pubsub:subscribe(GuildId, marvin_guild_pubsub:type_command(), marvin_plugin_command:short(command_mute())),
    {ok, #state{
        config = PluginConfig,
        guild_id = GuildId,
        active_mutes = ets:new(marvin_plugin_moderator_active_mutes, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ]),
        cleanup_timer = init_cleanup_timer()
    }}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(?cleanup_event(), S0) ->
    handle_info_cleanup_event(S0);

handle_info(Info, S0) ->
    try
        handle_info_guild_event(Info, S0)
    catch
        T:R ->
            marvin_log:error(
                "Guild '~s' plugin '~s' failed guild event handling with reason ~p:~p. ~p",
                [S0#state.guild_id, ?MODULE, T, R, erlang:get_stacktrace()]
            ),
            {noreply, S0}
    end.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



init_cleanup_timer() ->
    erlang:send_after(?cleanup_interval * 1000, self(), ?cleanup_event()).



handle_info_cleanup_event(S0) ->
    S1 = lists:foldl(fun handle_info_cleanup_event_maybe_mute_stop/2, S0, ets:tab2list(S0#state.active_mutes)),
    {noreply, S1#state{cleanup_timer = init_cleanup_timer()}}.



handle_info_cleanup_event_maybe_mute_stop(#active_mute{
    user_id = UserId, mute_till = MuteTill
} = _ActiveMute, S0) ->
    #{<<"role_mute">> := RoleMute} = marvin_plugin_config:data(S0#state.config),
    case os:system_time(second) >= MuteTill of
        true ->
            Req = marvin_rest_request:new(
                marvin_rest_impl_guild_member_role_delete,
                #{
                    <<"guild_id">> => S0#state.guild_id,
                    <<"user_id">> => UserId,
                    <<"role_id">> => RoleMute
                }, #{}
            ),
            Resp = marvin_rest:request(Req),
            marvin_log:info("Response: ~p", [Resp]);
        false ->
            ok
    end,
    S0.



handle_info_guild_event(Event, S0) ->
    TypeCommand = marvin_guild_pubsub:type_command(),
    ShortCommandMute = marvin_plugin_command:short(command_mute()),
    case {marvin_guild_pubsub:type(Event), marvin_guild_pubsub:action(Event)} of
        {TypeCommand, ShortCommandMute} ->
            handle_info_guild_event_command_mute(Event, S0);
        {_Type, _Action} ->
            marvin_log:warn(
                "Plugin '~s' for guild '~s' got unknown guild event: ~ts/~ts",
                [?MODULE, S0#state.guild_id, _Type, _Action]
            ),
            {noreply, S0}
    end.



handle_info_guild_event_command_mute(Event, S0) ->
    #{<<"role_mute">> := RoleMute} = marvin_plugin_config:data(S0#state.config),
    RoleModerator = marvin_guild_context:role_moderator(marvin_guild_pubsub:guild_context(Event)),
    case marvin_helper_chain:chain(
        'marvin_plugin_moderator:handle_info_guild_event_command_mute', [
            fun handle_info_guild_event_command_mute_provision_context/1,
            fun handle_info_guild_event_command_mute_ensure_caller_is_moderator/1,
            fun handle_info_guild_event_command_mute_filter_already_muted_subjects/1,
            fun handle_info_guild_event_command_mute_maybe_get_mute_duration/1
        ], #handle_info_guild_event_command_mute{
            event = Event, role_mute = RoleMute, role_moderator = RoleModerator
        }
    ) of
        {ok, ChainCtx} ->
            marvin_log:info("command successful: ~p", [ChainCtx]);
        {skip, {Reason, ChainCtx}} ->
            marvin_log:info("command skip due to ~p: ~p", [Reason, ChainCtx]);
        {error, Reason} ->
            marvin_log:info("command error: ~p", [Reason])
    end,
            % Req = marvin_rest_request:new(
            %     marvin_rest_impl_guild_member_role_add,
            %     #{
            %         <<"guild_id">> => S0#state.guild_id,
            %         <<"user_id">> => ActiveMute#active_mute.channel_id,
            %         <<"role_id">> => RoleMute
            %     }, #{}
            % ),
            % Resp = marvin_rest:request(Req),
            % marvin_log:info("Response: ~p", [Resp]),
            % insert_mute(S0#state.active_mutes, #active_mute{
            %     channel_name = ChannelName,
            %     origin_channel_id = marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
            %     owner = marvin_pdu2_dispatch_message_create:author(OriginalMessage),
            %     dialogflow_message_template = marvin_dialogflow_response_result:fulfillment(marvin_dialogflow_response:result(DialogFlowResponse))
            % }),
    {noreply, S0}.



handle_info_guild_event_command_mute_provision_context(
    #handle_info_guild_event_command_mute{event = Event} = ChainCtx
) ->
    #{original_message := OriginalMessage} = marvin_guild_pubsub:payload(Event),
    ChannelId = marvin_pdu2_dispatch_message_create:channel_id(OriginalMessage),
    Caller = marvin_guild_helper_members:r_get_member_by_user_id(
        marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(OriginalMessage)),
        marvin_guild_pubsub:guild_context(Event)
    ),
    Subjects = [marvin_guild_helper_members:r_get_member_by_user_id(
        marvin_pdu2_object_user:id(Subject),
        marvin_guild_pubsub:guild_context(Event)
    ) || Subject <- marvin_pdu2_dispatch_message_create:mentions(OriginalMessage)],
    {ok, ChainCtx#handle_info_guild_event_command_mute{
        channel_id = ChannelId, caller = Caller, subjects = Subjects
    }}.



handle_info_guild_event_command_mute_ensure_caller_is_moderator(
    #handle_info_guild_event_command_mute{
        caller = Caller, role_moderator = RoleModerator
    } = ChainCtx
) ->
    case marvin_pdu2_object_member:have_role(Caller, RoleModerator) of
        true ->
            {ok, ChainCtx};
        false ->
            {skip, {caller_is_not_moderator, ChainCtx}}
    end.



handle_info_guild_event_command_mute_filter_already_muted_subjects(
    #handle_info_guild_event_command_mute{
        subjects = Subjects0, role_mute = RoleMute
    } = ChainCtx
) ->
    case lists:foldl(fun(Subject, {Subjects, SubjectsMuted}) ->
        case marvin_pdu2_object_member:have_role(Subject, RoleMute) of
            true ->
                {Subjects, [Subject | SubjectsMuted]};
            false ->
                {[Subject | Subjects], SubjectsMuted}
        end
    end, {[], []}, Subjects0) of
        {[], SubjectsMuted1} ->
            {skip, {no_subject_to_mute, ChainCtx#handle_info_guild_event_command_mute{
                subjects = [], subjects_muted = SubjectsMuted1
            }}};
        {Subjects1, SubjectsMuted1} ->
            {ok, ChainCtx#handle_info_guild_event_command_mute{
                subjects = Subjects1, subjects_muted = SubjectsMuted1
            }}
    end.



handle_info_guild_event_command_mute_maybe_get_mute_duration(
    #handle_info_guild_event_command_mute{event = Event} = ChainCtx
) ->
    #{dialogflow_response := DialogFlowResponse} = marvin_guild_pubsub:payload(Event),
    Duration = case maps:find(<<"n_minutes">>, marvin_dialogflow_response_result:parameters(
        marvin_dialogflow_response:result(DialogFlowResponse)
    )) of
        {ok, DetectedDuration} -> DetectedDuration;
        error -> ?default_duration
    end,
    {ok, ChainCtx#handle_info_guild_event_command_mute{duration = Duration}}.







% lookup_mute(Ets, ChannelName) ->
%     case ets:lookup(Ets, ChannelName) of
%         [Channel] ->
%             {ok, Channel};
%         [] ->
%             {error, not_found}
%     end.



% insert_mute(Ets, Channel) ->
%     ets:insert(Ets, Channel),
%     ok.



% delete_mute(Ets, ChannelName) ->
%     ets:delete(Ets, ChannelName),
%     ok.
