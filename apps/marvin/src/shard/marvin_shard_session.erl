-module(marvin_shard_session).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/3, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([report_operational/3, incoming_event/2]).



-define(start_connection(), {start_connection}).
-define(report_operational(RxPid, TxPid), {report_operational, RxPid, TxPid}).
-define(incoming_event(Event), {incoming_event, Event}).

-record(state, {
    shard_id :: non_neg_integer(),
    shard_name :: atom(),
    wss_url :: binary(),
    rx_pid :: pid() | undefined,
    tx_pid :: pid() | undefined,
    heart_pid :: pid() | undefined,
    last_seq :: non_neg_integer(),
    session_id :: binary() | undefined,
    user :: marvin_pdu2_object_user:object() | unedefined
}).
-type state() :: #state{}.



%% Interface



-spec report_operational(SessionPid :: pid(), RxPid :: pid(), TxPid :: pid()) ->
    marvin_helper_type:ok_return().

report_operational(SessionPid, RxPid, TxPid) ->
    gen_server:call(SessionPid, ?report_operational(RxPid, TxPid)),
    ok.



-spec incoming_event(SessionPid :: pid(), Event :: binary()) ->
    marvin_helper_type:ok_return().

incoming_event(SessionPid, Event) ->
    gen_server:call(SessionPid, ?incoming_event(Event)),
    ok.



-spec start_link(
    ShardId :: non_neg_integer(),
    ShardName :: atom(),
    WssUrl :: binary()
) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(ShardId, ShardName, WssUrl) ->
    gen_server:start_link({local, ShardName}, ?MODULE, [ShardId, ShardName, WssUrl], []).



init([ShardId, ShardName, WssUrl]) ->
    erlang:process_flag(trap_exit, true),
    self() ! ?start_connection(),
    {ok, #state{
        shard_id = ShardId,
        shard_name = ShardName,
        wss_url = WssUrl,
        last_seq = 0
    }}.



handle_call(?report_operational(RxPid, TxPid), _GenReplyTo, S0) ->
    handle_call_report_operational(RxPid, TxPid, S0);

handle_call(?incoming_event(Event), _GenReplyTo, S0)
when is_binary(Event) ->
    handle_call_incoming_event(Event, S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(?start_connection(), S0) ->
    handle_info_start_connection(S0);

handle_info({'EXIT', ExitPid, ExitReason}, S0) ->
    handle_info_exit(ExitPid, ExitReason, S0);

handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec handle_info_start_connection(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_start_connection(#state{
    shard_id = ShardId,
    wss_url = WssUrl
} = S0) ->
    {ok, RxPid} = marvin_shard_sup:start_shard_rx(ShardId, self(), WssUrl),
    true = erlang:link(RxPid),
    {noreply, S0}.



-spec handle_call_report_operational(RxPid :: pid(), TxPid :: pid(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_report_operational(RxPid, TxPid, S0) ->
    marvin_log:debug("Shard '~p' is in operational state", [S0#state.shard_name]),
    true = erlang:link(RxPid),
    {reply, ok, S0#state{
        rx_pid = RxPid,
        tx_pid = TxPid
    }}.



-spec handle_call_incoming_event(Event :: binary(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event(Event, S0) ->
    marvin_log:debug(
        "Shard '~p' got incoming event (~p bytes)",
        [S0#state.shard_name, byte_size(Event)]
    ),
    ParseStartTime = erlang:monotonic_time(),
    case marvin_pdu2:parse(Event) of
        {ok, Struct} ->
            prometheus_histogram:observe(
                marvin_shard_session_pdu_parse_seconds,
                [S0#state.shard_name, marvin_pdu2:prot_mod(Struct)],
                erlang:monotonic_time() - ParseStartTime
            ),
            case marvin_pdu2:prot_mod(Struct) of
                marvin_pdu2_hello ->
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_hello]),
                    handle_call_incoming_event_hello(marvin_pdu2:d(Struct), S0);
                marvin_pdu2_invalid_session ->
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_hello]),
                    handle_call_incoming_event_invalid_session(marvin_pdu2:d(Struct), S0);
                marvin_pdu2_dispatch_resumed ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_resumed]),
                    handle_call_incoming_event_dispatch_resumed(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_heartbeat_ack ->
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_heartbeat_ack]),
                    handle_call_incoming_event_heartbeat_ack(marvin_pdu2:d(Struct), S0);
                marvin_pdu2_dispatch_ready ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_ready]),
                    handle_call_incoming_event_dispatch_ready(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_create ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_create]),
                    handle_call_incoming_event_dispatch_guild_create(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_members_chunk ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_members_chunk]),
                    handle_call_incoming_event_dispatch_guild_members_chunk(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_presence_update ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_presence_update]),
                    handle_call_incoming_event_dispatch_presence_update(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_message_create ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_message_create]),
                    handle_call_incoming_event_dispatch_message_create(marvin_pdu2:d(Struct), S1);
                _ ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_generic]),
                    handle_call_incoming_event_generic(Struct, S1)
            end;
        {error, Reason} ->
            prometheus_histogram:observe(
                marvin_shard_session_pdu_parse_seconds,
                [S0#state.shard_name, error],
                erlang:monotonic_time() - ParseStartTime
            ),
            marvin_log:error("Incoming PDU failed to parse due to reason: ~p, ignoring", [Reason]),
            {reply, ok, S0}
    end.



-spec handle_info_exit(
    ExitPid :: pid(),
    ExitReason :: term(),
    State :: state()
) ->
    marvin_helper_type:gen_server_noreply(State :: state()) |
    marvin_helper_type:gen_server_stop_noreply(State :: state()).

handle_info_exit(ExitPid, ExitReason, #state{
    rx_pid = ExitPid
} = S0) ->
    marvin_log:alert(
        "Shard '~p' reporting connection down with reason '~p', restarting",
        [S0#state.shard_name, ExitReason]
    ),
    self() ! ?start_connection(),
    {noreply, S0#state{
        rx_pid = undefined,
        tx_pid = undefined
    }};

handle_info_exit(ExitPid, ExitReason, #state{
    heart_pid = ExitPid
} = S0) ->
    marvin_log:alert(
        "Shard '~p' reporting heart down with reason '~p'",
        [S0#state.shard_name, ExitReason]
    ),
    {noreply, S0#state{
        rx_pid = undefined,
        tx_pid = undefined
    }};

handle_info_exit(ExitPid, ExitReason, #state{
    rx_pid = RxLinkRef
} = S0) when RxLinkRef =/= ExitPid ->
    marvin_log:alert(
        "Shard '~p' got unexpected 'EXIT' with reason '~p' from ~p, shutting down ",
        [S0#state.shard_name, ExitReason, ExitPid]
    ),
    {stop, shutdown, S0}.



%% Event handlers



-spec handle_call_incoming_event_hello(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_hello(Struct, #state{
    shard_id = ShardId,
    tx_pid = TxPid,
    session_id = undefined
} = S0) ->
    HeartbeatInterval = marvin_pdu2_hello:heartbeat_interval(Struct),
    marvin_log:info(
        "Shard '~p' starting heartbeat with interval '~p'",
        [S0#state.shard_name, HeartbeatInterval]
    ),
    {ok, HeartPid} = marvin_shard_sup:start_shard_heart(ShardId, TxPid, HeartbeatInterval),
    erlang:link(HeartPid),
    {ok, IdentifyEvent} = get_pdu_identify(S0),
    ok = marvin_shard_tx:send_sync(TxPid, IdentifyEvent),
    {reply, ok, S0#state{
        heart_pid = HeartPid
    }};

handle_call_incoming_event_hello(Struct, #state{
    shard_id = ShardId,
    tx_pid = TxPid
} = S0) ->
    HeartbeatInterval = marvin_pdu2_hello:heartbeat_interval(Struct),
    marvin_log:info(
        "Shard '~p' starting heartbeat with interval '~p'",
        [S0#state.shard_name, HeartbeatInterval]
    ),
    {ok, HeartPid} = marvin_shard_sup:start_shard_heart(ShardId, TxPid, HeartbeatInterval),
    erlang:link(HeartPid),
    {ok, ResumeEvent} = get_pdu_resume(S0),
    ok = marvin_shard_tx:send_sync(TxPid, ResumeEvent),
    {reply, ok, S0#state{
        heart_pid = HeartPid
    }}.



-spec handle_call_incoming_event_invalid_session(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_stop_simple(State :: state()).

handle_call_incoming_event_invalid_session(_Struct, S0) ->
    marvin_log:error(
        "Shard '~p' invalid session event, performing controlled crash",
        [S0#state.shard_name]
    ),
    {stop, invalid_session, ok, S0}.



-spec handle_call_incoming_event_dispatch_resumed(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_resumed(_Struct, #state{
    session_id = SessionId
} = S0) ->
    marvin_log:info(
        "Shard '~p' successfully resumed session '~s'",
        [S0#state.shard_name, SessionId]
    ),
    {reply, ok, S0#state{}}.



-spec handle_call_incoming_event_heartbeat_ack(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_heartbeat_ack(_Struct, #state{
    heart_pid = HeartPid
} = S0) ->
    marvin_log:debug("Shard '~p' got heartbeat ack", [S0#state.shard_name]),
    marvin_shard_heart:heartbeat_ack(HeartPid),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_ready(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_ready(Struct, #state{
    tx_pid = TxPid
} = S0) ->
    SessionId = marvin_pdu2_dispatch_ready:session_id(Struct),
    SelfUser = marvin_pdu2_dispatch_ready:user(Struct),
    marvin_log:info(
        "Shard '~p' is ready with session '~s'",
        [S0#state.shard_name, SessionId]
    ),
    {ok, StatusUpdate} = get_pdu_status_update(S0),
    ok = marvin_shard_tx:send_sync(TxPid, StatusUpdate),
    _ = marvin_helper_chain:chain(
        'marvin_shard_session:handle_call_incoming_event_dispatch_ready', [
            fun handle_call_incoming_event_dispatch_ready_start_guilds_get_ids/1,
            fun handle_call_incoming_event_dispatch_ready_start_guilds_maybe_start_guilds/1
        ], Struct),
    {reply, ok, S0#state{
        session_id = SessionId,
        user = SelfUser
    }}.



-spec handle_call_incoming_event_dispatch_ready_start_guilds_get_ids(
    Struct :: marvin_pdu2:t()
) ->
    Ret :: {ok, [GuildId :: non_neg_integer()]}.

handle_call_incoming_event_dispatch_ready_start_guilds_get_ids(Struct) ->
    {ok, lists:map(
        fun marvin_pdu2_object_guild_unavailable:id/1,
        marvin_pdu2_dispatch_ready:guilds(Struct)
    )}.



-spec handle_call_incoming_event_dispatch_ready_start_guilds_maybe_start_guilds(
    Ids :: [GuildId :: non_neg_integer()]
) ->
    Ret :: {ok, [Pid :: pid()]}.

handle_call_incoming_event_dispatch_ready_start_guilds_maybe_start_guilds(Ids) ->
    {ok, lists:map(
        fun marvin_helper_chain:unwrap2/1,
        lists:map(fun marvin_guild_monitor:maybe_start_guild/1, Ids)
    )}.



-spec handle_call_incoming_event_dispatch_guild_create(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_guild_create(Struct, #state{
    tx_pid = TxPid
} = S0) ->
    GuildId = marvin_pdu2_dispatch_guild_create:id(Struct),
    marvin_log:info(
        "Shard '~p' is provisioning the guild '~s'",
        [S0#state.shard_name, GuildId]
    ),
    marvin_guild_monitor:maybe_start_guild(GuildId),
    {ok, RequestGuildMembers} = get_pdu_request_guild_members(GuildId, S0),
    ok = marvin_shard_tx:send_sync(TxPid, RequestGuildMembers),
    ok = marvin_guild:do_provision(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_guild_members_chunk(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_guild_members_chunk(Struct, S0) ->
    GuildId = marvin_pdu2_dispatch_guild_members_chunk:guild_id(Struct),
    marvin_log:info(
        "Shard '~p' is provisioning members for the guild '~s'",
        [S0#state.shard_name, GuildId]
    ),
    ok = marvin_guild:do_provision_guild_members(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_presence_update(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_presence_update(Struct, S0) ->
    GuildId = marvin_pdu2_dispatch_presence_update:guild_id(Struct),
    UserId = marvin_pdu2_dispatch_presence_update:user(Struct),
    marvin_log:debug(
        "Shard '~p' got presence update for guild '~s'/user '~s'",
        [S0#state.shard_name, GuildId, UserId]
    ),
    ok = marvin_guild:presence_update(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_message_create(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_message_create(Struct, S0) ->
    ChannelId = marvin_pdu2_dispatch_message_create:channel_id(Struct),
    case marvin_channel_registry:lookup(ChannelId) of
        {ok, GuildId} ->
            marvin_log:debug(
                "Shard '~p' got message create for guild '~s'/channel '~s'",
                [S0#state.shard_name, GuildId, ChannelId]
            ),
            ok = marvin_guild:message_create(GuildId, Struct);
        {error, not_found} ->
            marvin_log:debug(
                "Shard '~p' got message create for unknown channel '~s'",
                [S0#state.shard_name, ChannelId]
            )
    end,
    {reply, ok, S0}.



-spec handle_call_incoming_event_generic(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_generic(Struct, S0) ->
    marvin_log:warn(
        "Shard '~p' got unhandled pdu of type '~p'",
        [S0#state.shard_name, marvin_pdu2:prot_mod(Struct)]
    ),
    {reply, ok, S0}.



-spec get_pdu_identify(State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: binary()).

get_pdu_identify(#state{
    shard_id = ShardId
} = S0) ->
    {ok, Token} = marvin_config:get(marvin, [discord, token]),
    OS = list_to_binary(lists:takewhile(
        fun($\n) -> false; (_) -> true end,
        os:cmd("uname -rs"))
    ),
    {ok, Compress} = marvin_config:get_boolean(marvin, [discord, gateway, compress]),
    {ok, LargeThreshold} = marvin_config:get_integer(marvin, [discord, gateway, large_threshold]),
    {ok, LibraryWeb} = marvin_config:get(marvin, [system_info, library_web]),
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    Library = <<LibraryName/binary, "/", LibraryVersion/binary>>,
    Shard = [ShardId, marvin_gateway_meta:get_shards_count()],
    marvin_log:debug(
        "Shard '~p' is identifying against discord server as '~s'",
        [S0#state.shard_name, Library]
    ),
    marvin_pdu2:render(marvin_pdu2_identify:new(#{
        token => Token,
        compress => Compress,
        large_threshold => LargeThreshold,
        shard => Shard,
        properties => #{
            '$os' => OS,
            '$browser' => Library,
            '$device' => Library,
            '$referrer' => LibraryWeb,
            '$referring_domain' => LibraryWeb
        }
    })).



-spec get_pdu_request_guild_members(GuildId :: marvin_pdu2:snowflake(), State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: binary()).

get_pdu_request_guild_members(GuildId, S0) ->
    marvin_log:debug(
        "Shard '~p' is requesting guild members for guild '~s'",
        [S0#state.shard_name, GuildId]
    ),
    marvin_pdu2:render(marvin_pdu2_request_guild_members:new(#{
        guild_id => GuildId,
        query => <<>>,
        limit => 0
    })).



-spec get_pdu_status_update(State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: binary()).

get_pdu_status_update(_S0) ->
    {ok, Version} = marvin_config:get(marvin, [system_info, library_version]),
    marvin_pdu2:render(marvin_pdu2_status_update:new(#{
        game => #{type => 0, name => Version},
        status => <<"online">>
    })).



-spec get_pdu_resume(State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: binary()).

get_pdu_resume(#state{
    session_id = SessionId,
    last_seq = LastSeq
} = S0) ->
    {ok, Token} = marvin_config:get(marvin, [discord, token]),
    marvin_log:debug(
        "Shard '~p' is resuming session '~s'",
        [S0#state.shard_name, SessionId]
    ),
    marvin_pdu2:render(marvin_pdu2_resume:new(#{
        token => Token, session_id => SessionId, seq => LastSeq
    })).



-spec maybe_bump_heart_seq(Seq :: non_neg_integer() | undefined, State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: state()).

maybe_bump_heart_seq(undefined, S0) ->
    {ok, S0};

maybe_bump_heart_seq(Seq, #state{
    heart_pid = HeartPid
} = S0) ->
    ok = marvin_shard_heart:bump_seq(HeartPid, Seq),
    {ok, S0#state{last_seq = Seq}}.
