-module(marvin_shard_rx).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/4, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).



-type internal_state() :: initial | gun_connect | gun_up | wss_up | pre_operational | operational.

-define(gun_connect(), {gun_connect}).
-define(gun_up(ConnPid, Proto), {gun_up, ConnPid, Proto}).
-define(gun_ws_upgrade(ConnPid, Ret, Headers),
    {gun_ws_upgrade, ConnPid, Ret, Headers}).
-define(start_tx(), {start_tx}).
-define(report_operational(), {report_operational}).
-define(gun_ws(WssPid, Type, Data), {gun_ws, WssPid, {Type, Data}}).
-define(gun_down(WssPid, Proto, Reason, KilledStreams, UnprocessedStreams),
    {gun_down, WssPid, Proto, Reason, KilledStreams, UnprocessedStreams}).

-record(state, {
    internal_state :: internal_state(),
    shard_id :: non_neg_integer(),
    shard_name :: atom(),
    session_pid :: pid(),
    wss_url :: binary(),
    wss_pid :: pid() | undefined,
    wss_reference :: reference() | undefined,
    tx_pid :: pid() | undefined
}).
-type state() :: #state{}.



%% Interface



-spec start_link(
    ShardId :: non_neg_integer(),
    ShardName :: atom(),
    SessionPid :: pid(),
    WssUrl :: binary()
) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(ShardId, ShardName, SessionPid, WssUrl) ->
    gen_server:start_link({local, ShardName}, ?MODULE, [ShardId, ShardName, SessionPid, WssUrl], []).



init([ShardId, ShardName, SessionPid, WssUrl]) ->
    self() ! ?gun_connect(),
    {ok, #state{
        internal_state = initial,
        shard_id = ShardId,
        shard_name = ShardName,
        session_pid = SessionPid,
        wss_url = WssUrl
    }}.



handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(?gun_connect(), S0) ->
    handle_info_gun_connect(S0);

handle_info(?gun_up(_ConnPid, _Proto), S0) ->
    handle_info_gun_up(S0);

handle_info(?gun_ws_upgrade(_ConnPid, _Ret, _Headers), S0) ->
    handle_info_gun_ws_upgrade(S0);

handle_info(?start_tx(), S0) ->
    handle_info_start_tx(S0);

handle_info(?report_operational(), S0) ->
    handle_info_report_operational(S0);

handle_info(?gun_ws(_WssPid, Type, Data), S0) ->
    handle_info_gun_ws(Type, Data, S0);

handle_info(?gun_down(WssPid, _Proto, Reason, _KilledStreams, _UnprocessedStreams), S0) ->
    handle_info_gun_down(WssPid, Reason, S0);

handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec handle_info_gun_connect(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()) | no_return().

handle_info_gun_connect(#state{
    internal_state = initial,
    wss_url = <<"wss://", WssHostBin/binary>>
} = S0) ->
    WssHost = binary_to_list(WssHostBin),
    {ok, WssPort} = marvin_config:get(marvin, [discord, gateway, port]),
    marvin_log:debug(
        "Shard '~p' connecting to wss endpoint '~s:~p'",
        [S0#state.shard_name, WssHost, WssPort]
    ),
    {ok, WssPid} = gun:open(WssHost, WssPort, #{transport => ssl, protocols => [http]}),
    {noreply, S0#state{
        internal_state = gun_connect,
        wss_pid = WssPid
    }};

handle_info_gun_connect(S0) ->
    controlled_crash(gun_connect, S0).



-spec handle_info_gun_up(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()) | no_return().

handle_info_gun_up(#state{
    internal_state = gun_connect,
    wss_pid = WssPid
} = S0) ->
    {ok, ProtoVersion} = marvin_config:get(marvin, [discord, gateway, protocol_version]),
    {ok, Compress} = marvin_config:get(marvin, [discord, gateway, compress]),
    marvin_log:debug(
        "Shard '~p' trying to upgrade connection to websocket state with opts: version '~s', compress ~p",
        [S0#state.shard_name, ProtoVersion, Compress]
    ),
    WssRef = gun:ws_upgrade(WssPid, "/?encoding=json&v=" ++ ProtoVersion, [], #{compress => Compress}),
    {noreply, S0#state{
        internal_state = gun_up,
        wss_reference = WssRef
    }};

handle_info_gun_up(S0) ->
    controlled_crash(gun_up, S0).



-spec handle_info_gun_ws_upgrade(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()) | no_return().

handle_info_gun_ws_upgrade(#state{
    internal_state = gun_up
} = S0) ->
    marvin_log:debug(
        "Shard '~p' successfully upgraded connection to websocket state",
        [S0#state.shard_name]
    ),
    self() ! ?start_tx(),
    {noreply, S0#state{
        internal_state = wss_up
    }};

handle_info_gun_ws_upgrade(S0) ->
    controlled_crash(ws_upgrade, S0).



-spec handle_info_start_tx(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()) | no_return().

handle_info_start_tx(#state{
    internal_state = wss_up,
    shard_id = ShardId,
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug("Shard '~p' starting tx", [S0#state.shard_name]),
    {ok, TxPid} = marvin_shard_sup:start_shard_tx(ShardId, self(), WssPid),
    true = erlang:link(TxPid),
    self() ! ?report_operational(),
    {noreply, S0#state{
        internal_state = pre_operational,
        tx_pid = TxPid
    }};

handle_info_start_tx(S0) ->
    controlled_crash(start_tx, S0).



-spec handle_info_report_operational(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()) | no_return().

handle_info_report_operational(#state{
    internal_state = pre_operational,
    session_pid = SessionPid,
    tx_pid = TxPid
} = S0) ->
    marvin_log:debug("Shard '~p' reporting operational state", [S0#state.shard_name]),
    marvin_shard_session:report_operational(SessionPid, self(), TxPid),
    {noreply, S0#state{internal_state = operational}};

handle_info_report_operational(S0) ->
    controlled_crash(report_operational, S0).



-spec handle_info_gun_ws(Type :: atom(), Data :: term(), State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_gun_ws(text, Data, #state{
    internal_state = operational,
    session_pid = SessionPid
} = S0) ->
    marvin_log:debug("Shard '~p' reporting incoming event ~p", [S0#state.shard_name, jiffy:decode(Data)]),
    marvin_shard_session:incoming_event(SessionPid, Data),
    {noreply, S0};

handle_info_gun_ws(binary, Data, #state{
    internal_state = operational
} = S0) ->
    marvin_log:debug(
        "Shard '~p' reporting incoming binary event of ~p bytes",
        [S0#state.shard_name, byte_size(Data)]
    ),
    {noreply, S0};

handle_info_gun_ws(Type, Data, #state{
    internal_state = InternalState,
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug(
        "Shard '~p' got incoming event of type '~p' while in state '~p', forwarding",
        [S0#state.shard_name, Type, InternalState]
    ),
    self() ! ?gun_ws(WssPid, Type, Data),
    {noreply, S0};

handle_info_gun_ws(Type, _Data, S0) ->
    marvin_log:warn(
        "Shard '~p' got incoming event of type '~p', ignoring",
        [S0#state.shard_name, Type]
    ),
    {noreply, S0}.



-spec handle_info_gun_down(WssPid :: pid(), Reason :: term(), State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()) | no_return().

handle_info_gun_down(WssPid, Reason, #state{
    wss_pid = WssPid
} = S0) ->
    controlled_crash({gun_down, Reason}, S0);

handle_info_gun_down(_SomePid, _Reason, #state{
    wss_pid = _WssPid
} = S0) ->
    {noreply, S0}.



-spec controlled_crash(Reason :: term(), State :: state()) ->
    no_return().

controlled_crash(Reason, #state{
    internal_state = InternalState
} = S0) ->
    marvin_log:alert(
        "Shard '~p' performing controlled crash with reason '~p' while in state '~p'",
        [S0#state.shard_name, Reason, InternalState]
    ),
    exit(controlled_crash).
