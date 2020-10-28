-module(marvin_shard_session).
-behaviour(gen_server).

-include_lib("marvin_log/include/marvin_log.hrl").
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
    user :: marvin_pdu2_object_user:object() | undefined
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
    try
        handle_call_incoming_event(Event, S0)
    catch Type:Reason:Stacktrace ->
        ?l_error(#{
            text => "Shard got error while handling event, ignoring",
            what => handle_call,
            details => #{
                shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                type => Type, reason => Reason, stacktrace => Stacktrace,
                link => 'https://github.com/shizzard/marvin/issues/31',
                event => Event
            }
        }),
        {reply, ok, S0}
    end;

handle_call(Unexpected, _GenReplyTo, S0) ->
    ?l_error(#{text => "Unexpected call", what => handle_call, details => Unexpected}),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected cast", what => handle_cast, details => Unexpected}),
    {noreply, S0}.



handle_info(?start_connection(), S0) ->
    handle_info_start_connection(S0);

handle_info({'EXIT', ExitPid, ExitReason}, S0) ->
    handle_info_exit(ExitPid, ExitReason, S0);

handle_info(Unexpected, S0) ->
    ?l_warning(#{text => "Unexpected info", what => handle_info, details => Unexpected}),
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
    ?l_debug(#{
        text => "Shard is starting rx",
        what => handle_info,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name
        }
    }),
    {ok, RxPid} = marvin_shard_sup:start_shard_rx(ShardId, self(), WssUrl),
    true = erlang:link(RxPid),
    {noreply, S0}.



-spec handle_call_report_operational(RxPid :: pid(), TxPid :: pid(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_report_operational(RxPid, TxPid, S0) ->
    ?l_debug(#{
        text => "Shard is in operational state",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name
        }
    }),
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
    ?l_debug(#{
        text => "Shard got incoming event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            data_size => byte_size(Event)
        }
    }),
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
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_invalid_session]),
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
                marvin_pdu2_dispatch_channel_create ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_channel_create]),
                    handle_call_incoming_event_dispatch_channel_create(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_channel_update ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_channel_update]),
                    handle_call_incoming_event_dispatch_channel_update(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_channel_delete ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_channel_delete]),
                    handle_call_incoming_event_dispatch_channel_delete(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_member_update ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_member_update]),
                    handle_call_incoming_event_dispatch_guild_member_update(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_role_create ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_role_create]),
                    handle_call_incoming_event_dispatch_guild_role_create(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_role_update ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_role_update]),
                    handle_call_incoming_event_dispatch_guild_role_update(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_role_delete ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_role_delete]),
                    handle_call_incoming_event_dispatch_guild_role_delete(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_guild_members_chunk ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_guild_members_chunk]),
                    handle_call_incoming_event_dispatch_guild_members_chunk(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_presence_update ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_presence_update]),
                    handle_call_incoming_event_dispatch_presence_update(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_typing_start ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_typing_start]),
                    handle_call_incoming_event_dispatch_typing_start(marvin_pdu2:d(Struct), S1);
                marvin_pdu2_dispatch_voice_state_update ->
                    {ok, S1} = maybe_bump_heart_seq(marvin_pdu2:s(Struct), S0),
                    prometheus_counter:inc(marvin_shard_session_incoming_events, [S0#state.shard_id, marvin_pdu2_dispatch_voice_state_update]),
                    handle_call_incoming_event_dispatch_voice_state_update(marvin_pdu2:d(Struct), S1);
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
            ?l_error(#{
                text => "Shard failed to parse incoming PDU",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    type => error, reason => Reason
                }
            }),
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
    ?l_alert(#{
        text => "Shard connection down, restarting",
        what => handle_info,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            reason => ExitReason
        }
    }),
    self() ! ?start_connection(),
    {noreply, S0#state{
        rx_pid = undefined,
        tx_pid = undefined
    }};

handle_info_exit(ExitPid, ExitReason, #state{
    heart_pid = ExitPid
} = S0) ->
    ?l_alert(#{
        text => "Shard heart down, restarting",
        what => handle_info,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            reason => ExitReason
        }
    }),
    {noreply, S0#state{
        rx_pid = undefined,
        tx_pid = undefined
    }};

handle_info_exit(ExitPid, ExitReason, #state{
    rx_pid = RxLinkRef
} = S0) when RxLinkRef =/= ExitPid ->
    ?l_alert(#{
        text => "Shard got unexpected 'EXIT', terminating",
        what => handle_info,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            pid => ExitPid, reason => ExitReason
        }
    }),
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
    tx_pid = TxPid
} = S0) ->
    HeartbeatInterval = marvin_pdu2_hello:heartbeat_interval(Struct),
    ?l_info(#{
        text => "Shard is starting heart",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            interval => HeartbeatInterval
        }
    }),
    {ok, HeartPid} = marvin_shard_sup:start_shard_heart(ShardId, TxPid, HeartbeatInterval),
    erlang:link(HeartPid),
    {ok, Event} = case S0#state.session_id of
        undefined -> get_pdu_identify(S0);
        _SessionId -> get_pdu_resume(S0)
    end,
    ok = marvin_shard_tx:send_sync(TxPid, Event),
    {reply, ok, S0#state{
        heart_pid = HeartPid
    }}.



-spec handle_call_incoming_event_invalid_session(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_stop_simple(State :: state()).

handle_call_incoming_event_invalid_session(_Struct, S0) ->
    ?l_info(#{
        text => "Shard got 'invalid session' event, terminate",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name
        }
    }),
    {stop, {shutdown, invalid_session}, ok, S0}.



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
    ?l_info(#{
        text => "Shard resumed session",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            session_id => SessionId
        }
    }),
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
    ?l_debug(#{
        text => "Shard got heartbeat ack",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name
        }
    }),
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
    SelfUserId = marvin_pdu2_object_user:id(SelfUser),
    ?l_info(#{
        text => "Shard is ready",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            session_id => SessionId
        }
    }),
    {ok, StatusUpdate} = get_pdu_status_update(S0),
    ok = marvin_shard_tx:send_sync(TxPid, StatusUpdate),
    _ = marvin_helper_chain:chain(
        'marvin_shard_session:handle_call_incoming_event_dispatch_ready', [
            fun handle_call_incoming_event_dispatch_ready_start_guilds_get_ids/1,
            fun handle_call_incoming_event_dispatch_ready_start_guilds_maybe_start_guilds/1
        ], {SelfUserId, Struct}),
    {reply, ok, S0#state{
        session_id = SessionId,
        user = SelfUser
    }}.



-spec handle_call_incoming_event_dispatch_ready_start_guilds_get_ids({
    SelfUserId :: marvin_pdu2:snowflake(),
    Struct :: marvin_pdu2:t()
}) ->
    Ret :: {ok, {
        SelfUserId :: marvin_pdu2:snowflake(),
        [GuildId :: non_neg_integer()]
    }}.

handle_call_incoming_event_dispatch_ready_start_guilds_get_ids({SelfUserId, Struct}) ->
    {ok, {SelfUserId, lists:map(
        fun marvin_pdu2_object_guild_unavailable:id/1,
        marvin_pdu2_dispatch_ready:guilds(Struct)
    )}}.



-spec handle_call_incoming_event_dispatch_ready_start_guilds_maybe_start_guilds({
    SelfUserId :: marvin_pdu2:snowflake(),
    Ids :: [GuildId :: non_neg_integer()]
}) ->
    Ret :: {ok, [Pid :: pid()]}.

handle_call_incoming_event_dispatch_ready_start_guilds_maybe_start_guilds({SelfUserId, Ids}) ->
    {ok, lists:map(
        fun marvin_helper_chain:unwrap2/1,
        [marvin_guild_monitor:maybe_start_guild(Id, SelfUserId) || Id <- Ids]
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
    tx_pid = TxPid,
    user = SelfUser
} = S0) ->
    SelfUserId = marvin_pdu2_object_user:id(SelfUser),
    GuildId = marvin_pdu2_dispatch_guild_create:id(Struct),
    ?l_info(#{
        text => "Shard is provisioning the guild",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            self_user_id => SelfUserId, guild_id => GuildId
        }
    }),
    marvin_guild_monitor:maybe_start_guild(GuildId, SelfUserId),
    {ok, RequestGuildMembers} = get_pdu_request_guild_members(GuildId, S0),
    ok = marvin_shard_tx:send_sync(TxPid, RequestGuildMembers),
    ok = marvin_guild:do_provision(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_channel_create(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_channel_create(Struct, S0) ->
    case marvin_pdu2_dispatch_channel_create:guild_id(Struct) of
        undefined ->
            ?l_warning(#{
                text => "Shard got 'channel create' event for unknown guild",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    channel_id => marvin_pdu2_dispatch_channel_create:id(Struct),
                    channel_type => marvin_pdu2_dispatch_channel_create:type(Struct),
                    channel_name => marvin_pdu2_dispatch_channel_create:name(Struct)
                }
            });
        GuildId ->
            ?l_debug(#{
                text => "Shard got 'channel create' event",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    guild_id => GuildId,
                    channel_id => marvin_pdu2_dispatch_channel_create:id(Struct),
                    channel_type => marvin_pdu2_dispatch_channel_create:type(Struct),
                    channel_name => marvin_pdu2_dispatch_channel_create:name(Struct)
                }
            }),
            ok = marvin_guild:channel_create(GuildId, Struct)
    end,
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_channel_update(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_channel_update(Struct, S0) ->
    case marvin_pdu2_dispatch_channel_update:guild_id(Struct) of
        undefined ->
            ?l_warning(#{
                text => "Shard got 'channel update' event for unknown guild",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    channel_id => marvin_pdu2_dispatch_channel_update:id(Struct),
                    channel_type => marvin_pdu2_dispatch_channel_update:type(Struct),
                    channel_name => marvin_pdu2_dispatch_channel_update:name(Struct)
                }
            });
        GuildId ->
            ?l_debug(#{
                text => "Shard got 'channel create' event",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    guild_id => GuildId,
                    channel_id => marvin_pdu2_dispatch_channel_update:id(Struct),
                    channel_type => marvin_pdu2_dispatch_channel_update:type(Struct),
                    channel_name => marvin_pdu2_dispatch_channel_update:name(Struct)
                }
            }),
            ok = marvin_guild:channel_update(GuildId, Struct)
    end,
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_channel_delete(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_channel_delete(Struct, S0) ->
    case marvin_pdu2_dispatch_channel_delete:guild_id(Struct) of
        undefined ->
            ?l_warning(#{
                text => "Shard got 'channel delete' event for unknown guild",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    channel_id => marvin_pdu2_dispatch_channel_delete:id(Struct),
                    channel_type => marvin_pdu2_dispatch_channel_delete:type(Struct),
                    channel_name => marvin_pdu2_dispatch_channel_delete:name(Struct)
                }
            });
        GuildId ->
            ?l_debug(#{
                text => "Shard got 'channel delete' event",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    guild_id => GuildId,
                    channel_id => marvin_pdu2_dispatch_channel_delete:id(Struct),
                    channel_type => marvin_pdu2_dispatch_channel_delete:type(Struct),
                    channel_name => marvin_pdu2_dispatch_channel_delete:name(Struct)
                }
            }),
            ok = marvin_guild:channel_delete(GuildId, Struct)
    end,
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_guild_member_update(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_guild_member_update(Struct, S0) ->
    GuildId = marvin_pdu2_dispatch_guild_member_update:guild_id(Struct),
    User = marvin_pdu2_dispatch_guild_member_update:user(Struct),
    UserId = marvin_pdu2_object_user:id(User),
    UserName = marvin_pdu2_object_user:username(User),
    ?l_debug(#{
        text => "Shard got 'member update' event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId, user_id => UserId, user_name => UserName
        }
    }),
    ok = marvin_guild:member_update(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_guild_role_create(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_guild_role_create(Struct, S0) ->
    GuildId = marvin_pdu2_dispatch_guild_role_create:guild_id(Struct),
    Role = marvin_pdu2_dispatch_guild_role_create:role(Struct),
    RoleId = marvin_pdu2_object_role:id(Role),
    RoleName = marvin_pdu2_object_role:name(Role),
    ?l_debug(#{
        text => "Shard got 'role create' event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId, role_id => RoleId, role_name => RoleName
        }
    }),
    ok = marvin_guild:role_create(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_guild_role_update(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_guild_role_update(Struct, S0) ->
    GuildId = marvin_pdu2_dispatch_guild_role_update:guild_id(Struct),
    Role = marvin_pdu2_dispatch_guild_role_update:role(Struct),
    RoleId = marvin_pdu2_object_role:id(Role),
    RoleName = marvin_pdu2_object_role:name(Role),
    ?l_debug(#{
        text => "Shard got 'role update' event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId, role_id => RoleId, role_name => RoleName
        }
    }),
    ok = marvin_guild:role_update(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_guild_role_delete(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_guild_role_delete(Struct, S0) ->
    GuildId = marvin_pdu2_dispatch_guild_role_delete:guild_id(Struct),
    RoleId = marvin_pdu2_dispatch_guild_role_delete:role_id(Struct),
    ?l_debug(#{
        text => "Shard got 'role delete' event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId, role_id => RoleId
        }
    }),
    ok = marvin_guild:role_delete(GuildId, Struct),
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
    ?l_info(#{
        text => "Shard is provisioning members",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId
        }
    }),
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
    ?l_debug(#{
        text => "Shard got 'presence update' event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId, user_id => UserId
        }
    }),
    ok = marvin_guild:presence_update(GuildId, Struct),
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_typing_start(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_typing_start(_Struct, S0) ->
    {reply, ok, S0}.



-spec handle_call_incoming_event_dispatch_voice_state_update(
    PDU :: marvin_pdu2:data(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_voice_state_update(Struct, S0) ->
    case marvin_pdu2_dispatch_voice_state_update:guild_id(Struct) of
        undefined ->
            ?l_warning(#{
                text => "Shard got 'voice state update' event for unknown guild",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    channel_id => marvin_pdu2_dispatch_voice_state_update:channel_id(Struct),
                    user_id => marvin_pdu2_dispatch_voice_state_update:user_id(Struct)
                }
            });
        GuildId ->
            ?l_debug(#{
                text => "Shard got 'voice state update' event",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    channel_id => marvin_pdu2_dispatch_voice_state_update:channel_id(Struct),
                    user_id => marvin_pdu2_dispatch_voice_state_update:user_id(Struct)
                }
            }),
            ok = marvin_guild:voice_state_update(GuildId, Struct)
    end,
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
            ?l_debug(#{
                text => "Shard got 'message create' event",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    guild_id => GuildId,
                    channel_id => marvin_pdu2_dispatch_message_create:channel_id(Struct),
                    user_id => marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(Struct))
                }
            }),
            ok = marvin_guild:message_create(GuildId, Struct);
        {error, not_found} ->
            ?l_warning(#{
                text => "Shard got 'message create' event for unknown guild",
                what => handle_call,
                details => #{
                    shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
                    channel_id => marvin_pdu2_dispatch_message_create:channel_id(Struct),
                    user_id => marvin_pdu2_object_user:id(marvin_pdu2_dispatch_message_create:author(Struct))
                }
            })
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
    ?l_warning(#{
        text => "Shard got unhandled event",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            event_type => marvin_pdu2:prot_mod(Struct)
        }
    }),
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
    IdentifyParams = #{
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
        },
        intents => marvin_pdu2_identify:create_intent([
            marvin_pdu2_identify:intent_guilds(),
            marvin_pdu2_identify:intent_guild_members(),
            marvin_pdu2_identify:intent_guild_bans(),
            marvin_pdu2_identify:intent_guild_emojis(),
            marvin_pdu2_identify:intent_guild_integrations(),
            marvin_pdu2_identify:intent_guild_webhooks(),
            marvin_pdu2_identify:intent_guild_invites(),
            marvin_pdu2_identify:intent_guild_voice_states(),
            marvin_pdu2_identify:intent_guild_presences(),
            marvin_pdu2_identify:intent_guild_messages()
        ])
    },
    ?l_info(#{
        text => "Shard is about to identify against discord server",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            library => Library, params => IdentifyParams
        }
    }),
    marvin_pdu2:render(marvin_pdu2_identify:new(IdentifyParams)).



-spec get_pdu_request_guild_members(GuildId :: marvin_pdu2:snowflake(), State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: binary()).

get_pdu_request_guild_members(GuildId, S0) ->
    ?l_debug(#{
        text => "Shard is requesting guild members",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            guild_id => GuildId
        }
    }),
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
    ?l_info(#{
        text => "Shard is about to resume session",
        what => handle_call,
        details => #{
            shard_id => S0#state.shard_id, shard_name => S0#state.shard_name,
            session_id => SessionId
        }
    }),
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
