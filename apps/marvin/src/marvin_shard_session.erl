-module(marvin_shard_session).
-behaviour(gen_server).

-include_lib("marvin_pdu/include/marvin_pdu.hrl").
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
    user :: marvin_pdu_object_user:object() | unedefined
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
    {ok, _RxPid} = marvin_shard_sup:start_shard_rx(ShardId, self(), WssUrl),
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
    case marvin_pdu:parse(Event) of
        {ok, {?marvin_pdu_dispatch_ready(_) = PDU0, Seq}} ->
            {ok, S1} = maybe_bump_heart_seq(Seq, S0),
            handle_call_incoming_event_dispatch_ready(PDU0, S1);
        {ok, {?marvin_pdu_dispatch_resumed(_) = PDU0, Seq}} ->
            {ok, S1} = maybe_bump_heart_seq(Seq, S0),
            handle_call_incoming_event_dispatch_resumed(PDU0, S1);
        {ok, {?marvin_pdu_hello(_) = PDU0, Seq}} ->
            {ok, S1} = maybe_bump_heart_seq(Seq, S0),
            handle_call_incoming_event_hello(PDU0, S1);
        {ok, {?marvin_pdu_heartbeat_ack(_) = PDU0, Seq}} ->
            {ok, S1} = maybe_bump_heart_seq(Seq, S0),
            handle_call_incoming_event_heartbeat_ack(PDU0, S1);
        {ok, {?marvin_pdu(_, _) = PDU0, Seq}} ->
            {ok, S1} = maybe_bump_heart_seq(Seq, S0),
            handle_call_incoming_event_generic(PDU0, S1);
        {error, Reason} ->
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



-spec handle_call_incoming_event_dispatch_ready(
    PDU :: marvin_pdu:pdu(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_ready(PDU0, S0) ->
    SessionId = marvin_pdu_dispatch_ready:session_id(PDU0),
    SelfUser = marvin_pdu_dispatch_ready:user(PDU0),
    marvin_log:info(
        "Shard '~p' is ready with session '~s'",
        [S0#state.shard_name, SessionId]
    ),
    {reply, ok, S0#state{
        session_id = SessionId,
        user = SelfUser
    }}.



-spec handle_call_incoming_event_dispatch_resumed(
    PDU :: marvin_pdu:pdu(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_dispatch_resumed(_PDU0, #state{
    session_id = SessionId
} = S0) ->
    marvin_log:info(
        "Shard '~p' successfully resumed session '~s'",
        [S0#state.shard_name, SessionId]
    ),
    {reply, ok, S0#state{}}.



-spec handle_call_incoming_event_hello(
    PDU :: marvin_pdu:pdu(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_hello(PDU0, #state{
    shard_id = ShardId,
    tx_pid = TxPid,
    session_id = undefined
} = S0) ->
    HeartbeatInterval = marvin_pdu_hello:heartbeat_interval(PDU0),
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

handle_call_incoming_event_hello(PDU0, #state{
    shard_id = ShardId,
    tx_pid = TxPid
} = S0) ->
    HeartbeatInterval = marvin_pdu_hello:heartbeat_interval(PDU0),
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



-spec handle_call_incoming_event_heartbeat_ack(
    PDU :: marvin_pdu:pdu(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_heartbeat_ack(_PDU0, #state{
    heart_pid = HeartPid
} = S0) ->
    marvin_log:debug("Shard '~p' got heartbeat ack", [S0#state.shard_name]),
    marvin_shard_heart:heartbeat_ack(HeartPid),
    {reply, ok, S0}.



-spec handle_call_incoming_event_generic(
    PDU :: marvin_pdu:pdu(),
    State :: state()
) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_incoming_event_generic(?marvin_pdu(Type, _) = _PDU0, S0) ->
    marvin_log:warn(
        "Shard '~p' got unhandled pdu of type '~p'",
        [S0#state.shard_name, Type]
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
    {ok, Compress} = marvin_config:get(marvin, [discord, gateway, compress]),
    {ok, LargeThreshold} = marvin_config:get(marvin, [discord, gateway, large_threshold]),
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    Library = <<LibraryName/binary, "/", LibraryVersion/binary>>,
    Shard = [ShardId, marvin_gateway_meta:get_shards_count()],
    marvin_log:debug(
        "Shard '~p' is identifying against discord server as ~s",
        [S0#state.shard_name, Library]
    ),
    marvin_pdu:render(marvin_pdu_identify:new(Token, OS, Library, Compress, LargeThreshold, Shard)).



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
    marvin_pdu:render(marvin_pdu_resume:new(Token, SessionId, LastSeq)).



-spec maybe_bump_heart_seq(Seq :: non_neg_integer() | undefined, State :: state()) ->
    marvin_helper_type:ok_return(OkRet :: state()).

maybe_bump_heart_seq(undefined, S0) ->
    {ok, S0};

maybe_bump_heart_seq(Seq, #state{
    heart_pid = HeartPid
} = S0) ->
    ok = marvin_shard_heart:bump_seq(HeartPid, Seq),
    {ok, S0#state{last_seq = Seq}}.
