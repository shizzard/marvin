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
    tx_pid :: pid() | undefined
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
    self() ! ?start_connection(),
    {ok, #state{
        shard_id = ShardId,
        shard_name = ShardName,
        wss_url = WssUrl
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
    {ok, _TxPid} = marvin_shard_sup:start_shard_rx(ShardId, self(), WssUrl),
    {noreply, S0}.



-spec handle_call_report_operational(RxPid :: pid(), TxPid :: pid(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_report_operational(RxPid, TxPid, S0) ->
    marvin_log:debug("Shard '~p' is in operational state", [S0#state.shard_name]),
    erlang:process_flag(trap_exit, true),
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
    {reply, ok, S0}.



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
    rx_pid = RxLinkRef
} = S0) when RxLinkRef =/= ExitPid ->
    marvin_log:alert(
        "Shard '~p' got unexpected 'EXIT' with reason '~p', shutting down",
        [S0#state.shard_name, ExitReason]
    ),
    {stop, shutdown, S0}.
