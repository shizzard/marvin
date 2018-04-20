-module(marvin_guild_helper_presence).

-include("marvin_guild_state.hrl").

-export([
    w_do_provision/2,
    w_presence_update/2,
    r_get_member_status/2
]).



%% Interface



-spec w_do_provision(Presences :: [marvin_pdu2_object_presence:t()], Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_do_provision(Presences, Ctx) ->
    marvin_log:info("Guild '~s' presences: ~p total", [marvin_guild_context:guild_id(Ctx), length(Presences)]),
    lists:foldl(fun w_do_provision_fold/2, Ctx, Presences),
    do_report_presence_state(Ctx),
    ok.



-spec w_presence_update(Presence :: marvin_pdu2_dispatch_presence_update:t(), Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_presence_update(Presence, Ctx) ->
    UserId = marvin_pdu2_dispatch_presence_update:user(Presence),
    Status = marvin_pdu2_dispatch_presence_update:status(Presence),
    OldStatus = r_get_member_status(UserId, Ctx),
    do_update_member_presence(UserId, OldStatus, Status, Ctx),
    do_report_presence_state(Ctx),
    ok.



-spec r_get_member_status(UserId :: marvin_pdu2:snowflake(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_pdu2_object_presence:status().

r_get_member_status(UserId, Ctx) ->
    case ets:lookup(marvin_guild_context:presence_state(Ctx), UserId) of
        [] ->
            <<"offline">>;
        [#presence{user_id = UserId, status = Status}] ->
            Status
    end.



%% Internals



-spec w_do_provision_fold(
    Presence :: marvin_pdu2_object_presence:t(),
    Ctx :: marvin_guild_context:t()
) ->
    Ret :: marvin_guild_context:t().

w_do_provision_fold(Presence, Ctx) ->
    UserId = marvin_pdu2_object_presence:user(Presence),
    Status = marvin_pdu2_object_presence:status(Presence),
    OldStatus = r_get_member_status(UserId, Ctx),
    do_update_member_presence(UserId, OldStatus, Status, Ctx),
    Ctx.



-spec do_update_member_presence(
    UserId :: marvin_pdu2_object_user:id(),
    OldStatus :: marvin_pdu2_object_presence:status(),
    Status :: marvin_pdu2_object_presence:status(),
    Ctx :: marvin_guild_context:t()
) ->
    marvin_helper_type:ok_return().

do_update_member_presence(UserId, OldStatus, Status, Ctx) ->
    case {OldStatus, Status} of
        {_, <<"offline">>} ->
            ets:delete(marvin_guild_context:presence_state(Ctx), UserId);
        {_, _} ->
            ets:insert(marvin_guild_context:presence_state(Ctx), #presence{user_id = UserId, status = Status})
    end,
    ok.



-spec do_report_presence_state(Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

do_report_presence_state(Ctx) ->
    {Online, Idle, Dnd} = lists:foldl(
        fun do_report_presence_state_fold/2, {0,0,0},
        ets:tab2list(marvin_guild_context:presence_state(Ctx))
    ),
    prometheus_gauge:set(marvin_guild_presence_state, [marvin_guild_context:guild_id(Ctx), online], Online),
    prometheus_gauge:set(marvin_guild_presence_state, [marvin_guild_context:guild_id(Ctx), idle], Idle),
    prometheus_gauge:set(marvin_guild_presence_state, [marvin_guild_context:guild_id(Ctx), dnd], Dnd),
    ok.



-spec do_report_presence_state_fold(
    PresenceState :: #presence{},
    {Online :: non_neg_integer(), Idle :: non_neg_integer(), Dnd :: non_neg_integer()}
) ->
    Ret :: {Online :: non_neg_integer(), Idle :: non_neg_integer(), Dnd :: non_neg_integer()}.

do_report_presence_state_fold(#presence{status = <<"online">>}, {Online, Idle, Dnd}) ->
    {Online + 1, Idle, Dnd};

do_report_presence_state_fold(#presence{status = <<"idle">>}, {Online, Idle, Dnd}) ->
    {Online, Idle + 1, Dnd};

do_report_presence_state_fold(#presence{status = <<"dnd">>}, {Online, Idle, Dnd}) ->
    {Online, Idle, Dnd + 1}.
