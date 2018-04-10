-module(marvin_guild_helper_presence).

-include("marvin_guild_state.hrl").

-export([
    handle_call_do_provision_chain/1,
    handle_call_presence_update_chain/1,
    get_member_status/2
]).



%% Interface



-spec handle_call_do_provision_chain({Struct :: marvin_pdu2_dispatch_guild_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_guild_create:t(),
        S1 :: state()
    }).

handle_call_do_provision_chain({Struct, S0}) ->
    Presences = marvin_pdu2_dispatch_guild_create:presences(Struct),
    marvin_log:info("Guild '~s' presences: ~p total", [S0#state.guild_id, length(Presences)]),
    S1 = lists:foldl(fun do_update_member_presence_from_object/2, S0, Presences),
    do_report_presence_state(S1),
    {ok, {Struct, S1}}.



-spec handle_call_presence_update_chain({Struct :: marvin_pdu2_dispatch_presence_update:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_presence_update:t(),
        S1 :: state()
    }).

handle_call_presence_update_chain({Struct, S0}) ->
    UserId = marvin_pdu2_dispatch_presence_update:user(Struct),
    Status = marvin_pdu2_dispatch_presence_update:status(Struct),
    OldStatus = get_member_status(UserId, S0),
    S1 = do_update_member_presence(UserId, OldStatus, Status, S0),
    do_report_presence_state(S1),
    {ok, {Struct, S1}}.



-spec get_member_status(UserId :: marvin_pdu2:snowflake(), S0 :: state()) ->
    Ret :: marvin_pdu2_object_presence:status().

get_member_status(UserId, #state{
    presence_state = PresenceStateEts
}) ->
    case ets:lookup(PresenceStateEts, UserId) of
        [] ->
            <<"offline">>;
        [#presence{user_id = UserId, status = Status}] ->
            Status
    end.



%% Internals



-spec do_update_member_presence_from_object(Struct :: marvin_pdu2_object_presence:t(), S0 :: state()) ->
    Ret :: state().

do_update_member_presence_from_object(Struct, S0) ->
    UserId = marvin_pdu2_object_presence:user(Struct),
    Status = marvin_pdu2_object_presence:status(Struct),
    OldStatus = get_member_status(UserId, S0),
    do_update_member_presence(UserId, OldStatus, Status, S0).



-spec do_update_member_presence(
    UserId :: marvin_pdu2_object_user:id(),
    OldStatus :: marvin_pdu2_object_presence:status(),
    Status :: marvin_pdu2_object_presence:status(),
    S0 :: state()
) ->
    Ret :: state().

do_update_member_presence(UserId, OldStatus, Status, #state{
    presence_state = PresenceStateEts,
    members_online = MembersOnline,
    members_idle = MembersIdle,
    members_dnd = MembersDnd
} = S0) ->
    case {OldStatus, Status} of
        {<<"online">>, <<"online">>} ->
            S0;
        {<<"idle">>, <<"online">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_online = MembersOnline + 1, members_idle = MembersIdle - 1};
        {<<"dnd">>, <<"online">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_online = MembersOnline + 1, members_dnd = MembersDnd - 1};
        {<<"offline">>, <<"online">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_online = MembersOnline + 1};

        {<<"online">>, <<"idle">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_idle = MembersIdle + 1, members_online = MembersOnline - 1};
        {<<"idle">>, <<"idle">>} ->
            S0;
        {<<"dnd">>, <<"idle">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_idle = MembersIdle + 1, members_dnd = MembersDnd - 1};
        {<<"offline">>, <<"idle">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_idle = MembersIdle + 1};

        {<<"online">>, <<"dnd">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_dnd = MembersDnd + 1, members_online = MembersOnline - 1};
        {<<"idle">>, <<"dnd">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_dnd = MembersDnd + 1, members_idle = MembersIdle - 1};
        {<<"dnd">>, <<"dnd">>} ->
            S0;
        {<<"offline">>, <<"dnd">>} ->
            ets:insert(PresenceStateEts, #presence{user_id = UserId, status = Status}),
            S0#state{members_dnd = MembersDnd + 1};

        {<<"online">>, <<"offline">>} ->
            ets:delete(PresenceStateEts, UserId),
            S0#state{members_online = MembersOnline - 1};
        {<<"idle">>, <<"offline">>} ->
            ets:delete(PresenceStateEts, UserId),
            S0#state{members_idle = MembersIdle - 1};
        {<<"dnd">>, <<"offline">>} ->
            ets:delete(PresenceStateEts, UserId),
            S0#state{members_dnd = MembersDnd - 1};
        {<<"offline">>, <<"offline">>} ->
            S0
    end.



-spec do_report_presence_state(S0 :: state()) ->
    marvin_helper_type:ok_return().

do_report_presence_state(#state{
    guild_id = GuildId,
    members_online = MembersOnline,
    members_idle = MembersIdle,
    members_dnd = MembersDnd
}) ->
    prometheus_gauge:set(marvin_guild_presence_state, [GuildId, online], MembersOnline),
    prometheus_gauge:set(marvin_guild_presence_state, [GuildId, idle], MembersIdle),
    prometheus_gauge:set(marvin_guild_presence_state, [GuildId, dnd], MembersDnd),
    ok.
