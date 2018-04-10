-module(marvin_shard_rx).
-behaviour(gen_statem).

-export([start_link/4, init/1, callback_mode/0, handle_event/4]).

-record(state, {
    shard_id :: non_neg_integer(),
    shard_name :: atom(),
    session_pid :: pid(),
    wss_url :: binary(),
    wss_pid :: pid() | undefined,
    wss_reference :: reference() | undefined,
    tx_pid :: pid() | undefined
}).

-define(gun_connect(), {gun_connect}).
-define(gun_up(ConnPid, Proto), {gun_up, ConnPid, Proto}).
-define(gun_ws_upgrade(ConnPid, Ret, Headers),
    {gun_ws_upgrade, ConnPid, Ret, Headers}).
-define(start_tx(), {start_tx}).
-define(report_operational(), {report_operational}).
-define(gun_ws_data(WssPid, Type, Data), {gun_ws, WssPid, {Type, Data}}).
-define(gun_ws_close(WssPid, Code, Data), {gun_ws, WssPid,{close, Code, Data}}).
-define(gun_down(WssPid, Proto, Reason, KilledStreams, UnprocessedStreams),
    {gun_down, WssPid, Proto, Reason, KilledStreams, UnprocessedStreams}).



%% Interface


-spec start_link(
    ShardId :: non_neg_integer(),
    ShardName :: atom(),
    SessionPid :: pid(),
    WssUrl :: binary()
) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(ShardId, ShardName, SessionPid, WssUrl) ->
    gen_statem:start_link({local, ShardName}, ?MODULE, [ShardId, ShardName, SessionPid, WssUrl], []).


init([ShardId, ShardName, SessionPid, WssUrl]) ->
    {ok, on_start, #state{
        shard_id = ShardId,
        shard_name = ShardName,
        session_pid = SessionPid,
        wss_url = WssUrl
    }, {state_timeout, 0, ?gun_connect()}}.


callback_mode() ->
    handle_event_function.


%% States

% Initial state.
% Connecting to websocket endpoint.

handle_event(state_timeout, ?gun_connect(), on_start, #state{
    wss_url = <<"wss://", WssHostBin/binary>>
} = S0) ->
    WssHost = binary_to_list(WssHostBin),
    {ok, WssPort} = marvin_config:get_integer(marvin, [discord, gateway, port]),
    marvin_log:debug(
        "Shard '~p' connecting to wss endpoint '~s:~p'",
        [S0#state.shard_name, WssHost, WssPort]
    ),
    {ok, WssPid} = gun:open(WssHost, WssPort, #{transport => ssl, protocols => [http]}),
    {next_state, on_gun_connect, S0#state{
        wss_pid = WssPid
    }};

% Handling gun event 'connection up'.
% Upgrading connection to websocket state.

handle_event(info, ?gun_up(_ConnPid, _Proto), on_gun_connect, #state{
    wss_pid = WssPid
} = S0) ->
    {ok, ProtoVersion} = marvin_config:get(marvin, [discord, gateway, protocol_version]),
    {ok, Compress} = marvin_config:get_boolean(marvin, [discord, gateway, compress]),
    marvin_log:debug(
        "Shard '~p' trying to upgrade connection to websocket state with opts: version '~s', compress '~p'",
        [S0#state.shard_name, ProtoVersion, Compress]
    ),
    WssRef = gun:ws_upgrade(WssPid, "/?encoding=json&v=" ++ ProtoVersion, [], #{compress => Compress}),
    {next_state, on_gun_up, S0#state{
        wss_reference = WssRef
    }};

% Handling gun event 'websocket up'.
% Switching to 'start_tx' state immedeately.

handle_event(info, ?gun_ws_upgrade(_ConnPid, _Ret, _Headers), on_gun_up, S0) ->
    marvin_log:debug(
        "Shard '~p' successfully upgraded connection to websocket state",
        [S0#state.shard_name]
    ),
    {next_state, on_wss_up, S0, {state_timeout, 0, ?start_tx()}};

% Starting pairing `marvin_shard_tx`.
% Switching to 'on_pre_operational' state immediately.

handle_event(state_timeout, ?start_tx(), on_wss_up, #state{
    shard_id = ShardId,
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug("Shard '~p' starting tx", [S0#state.shard_name]),
    {ok, TxPid} = marvin_shard_sup:start_shard_tx(ShardId, self(), WssPid),
    true = erlang:link(TxPid),
    {next_state, on_pre_operational, S0#state{
        tx_pid = TxPid
    }, {state_timeout, 0, ?report_operational()}};

% Reporting operational state to parent `marvin_shard_session`.
% Switching to operational state.

handle_event(state_timeout, ?report_operational(), on_pre_operational, #state{
    session_pid = SessionPid,
    tx_pid = TxPid
} = S0) ->
    marvin_log:debug("Shard '~p' reporting operational state", [S0#state.shard_name]),
    marvin_shard_session:report_operational(SessionPid, self(), TxPid),
    {next_state, on_operational, S0};

% Handling text websocket data, passing it to parent `marvin_shard_session`.
% Keeping state and data.

handle_event(info, ?gun_ws_data(WssPid, text, Data), on_operational, #state{
    session_pid = SessionPid,
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug("Shard '~p' reporting incoming event ~p", [S0#state.shard_name, jiffy:decode(Data, [return_maps])]),
    marvin_shard_session:incoming_event(SessionPid, Data),
    keep_state_and_data;

% Handling binary websocket data, dropping it.
% Keeping state and data.

handle_event(info, ?gun_ws_data(WssPid, binary, Data), on_operational, #state{
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug(
        "Shard '~p' reporting incoming binary event of ~p bytes",
        [S0#state.shard_name, byte_size(Data)]
    ),
    keep_state_and_data;

% Handling websocket data before 'on_operational' state.
% Resending it back to `self()` to handle in 'on_operational' state.
% NB: this may cause infinite data passing to `self()`.

handle_event(info, ?gun_ws_data(WssPid, Type, Data), WrongState, #state{
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug(
        "Shard '~p' got incoming event of type '~p' while in state '~p', forwarding",
        [S0#state.shard_name, Type, WrongState]
    ),
    self() ! ?gun_ws_data(WssPid, Type, Data),
    keep_state_and_data;

% Handling websocket connection closed.
% Exiting to pass control to supervision tree.

handle_event(info, ?gun_ws_close(WssPid, Code, Data), State, #state{
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug(
        "Shard '~p' got gun connection close with code '~p'/data '~s' while in state '~p', giving up",
        [S0#state.shard_name, Code, Data, State]
    ),
    {stop, gun_close};

% Handling websocket connection down.
% Exiting to pass control to supervision tree.

handle_event(info, ?gun_down(WssPid, _Proto, Reason, _KilledStreams, _UnprocessedStreams), State, #state{
    wss_pid = WssPid
} = S0) ->
    marvin_log:debug(
        "Shard '~p' got gun connection down with reason '~p' while in state '~p', giving up",
        [S0#state.shard_name, Reason, State]
    ),
    {stop, gun_down}.
