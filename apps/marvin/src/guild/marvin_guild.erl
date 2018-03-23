-module(marvin_guild).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    do_provision/2, presence_update/2,
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).


-record(presence, {
    user_id :: marvin_pdu2:snowflake(),
    status :: marvin_pdu2_object_presence:status()
}).
-record(state, {
    guild_id :: non_neg_integer(),
    presence_state :: ets:tid(),
    members_online = 0 :: non_neg_integer(),
    members_idle = 0 :: non_neg_integer(),
    members_dnd = 0 :: non_neg_integer()
}).
-type state() :: #state{}.

-define(do_provision(Struct), {do_provision, Struct}).
-define(presence_update(Struct), {presence_update, Struct}).



%% Interface



-spec start_link(GuildId :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(GuildId) ->
    gen_server:start_link(?MODULE, [GuildId], []).



-spec do_provision(GuildPid :: pid(), Struct :: marvin_pdu2_dispatch_guild_create:t()) ->
    Ret :: marvin_helper_type:ok_return().

do_provision(GuildPid, Struct) ->
    gen_server:call(GuildPid, ?do_provision(Struct)).



-spec presence_update(GuildPid :: pid(), Struct :: marvin_pdu2_dispatch_presence_update:t()) ->
    Ret :: marvin_helper_type:ok_return().

presence_update(GuildPid, Struct) ->
    gen_server:call(GuildPid, ?presence_update(Struct)).



init([GuildId]) ->
    {ok, #state{
        guild_id = GuildId,
        presence_state = ets:new(marvin_guild_presence_state, [
            set, protected, {keypos, 2}, {read_concurrency, true}
        ])
    }}.



handle_call(?do_provision(Struct), _GenReplyTo, S0) ->
    marvin_log:info("Guild '~s' is being provisioned", [S0#state.guild_id]),
    handle_call_do_provision(Struct, S0);

handle_call(?presence_update(Struct), _GenReplyTo, S0) ->
    marvin_log:info("Guild '~s' got presence update", [S0#state.guild_id]),
    handle_call_presence_update(Struct, S0);

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



%% Internals



-spec handle_call_do_provision(Struct :: marvin_pdu2_dispatch_guild_create:t(), S0 :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_do_provision(Struct, S0) ->
    S1 = lists:foldl(
        fun do_update_member_presence_from_object/2, S0,
        marvin_pdu2_dispatch_guild_create:presences(Struct)
    ),
    do_report_presence_state(S1),
    {reply, ok, S1}.



-spec handle_call_presence_update(Struct :: marvin_pdu2_dispatch_presence_update:t(), S0 :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_presence_update(Struct, S0) ->
    UserId = marvin_pdu2_dispatch_presence_update:user(Struct),
    Status = marvin_pdu2_dispatch_presence_update:status(Struct),
    OldStatus = get_member_status(UserId, S0),
    S1 = do_update_member_presence(UserId, OldStatus, Status, S0),
    do_report_presence_state(S1),
    {reply, ok, S1}.



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
