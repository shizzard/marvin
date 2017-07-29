-module(marvin_shard_heart).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/4, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([heartbeat_ack/1]).



-define(do_beat(), {do_beat}).
-define(heartbeat_ack(), {heartbeat_ack}).

-record(state, {
    shard_id :: non_neg_integer(),
    shard_name :: atom(),
    tx_pid :: pid(),
    heartbeat_interval :: non_neg_integer(),
    last_seq :: non_neg_integer(),
    timer_reference :: timer:tref(),
    waiting_for_ack :: boolean()
}).
-type state() :: #state{}.



%% Interface



-spec heartbeat_ack(HeartPid :: pid()) ->
    Reply :: marvin_helper_type:ok_return().

heartbeat_ack(HeartPid) ->
    gen_server:call(HeartPid, ?heartbeat_ack()).



-spec start_link(
    ShardId :: non_neg_integer(),
    ShardName :: atom(),
    TxPid :: pid(),
    HeartbeatInterval :: pid()
) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(ShardId, ShardName, TxPid, HeartbeatInterval) ->
    gen_server:start_link({local, ShardName}, ?MODULE, [ShardId, ShardName, TxPid, HeartbeatInterval], []).



init([ShardId, ShardName, TxPid, HeartbeatInterval]) ->
    erlang:link(TxPid),
    {ok, TRef} = schedule_beat(0),
    {ok, #state{
        shard_id = ShardId,
        shard_name = ShardName,
        tx_pid = TxPid,
        heartbeat_interval = HeartbeatInterval,
        last_seq = 0,
        timer_reference = TRef,
        waiting_for_ack = false
    }}.



handle_call(?heartbeat_ack(), _GenReplyTo, S0) ->
    handle_call_heartbeat_ack(S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(Unexpected, S0) ->
    marvin_log:warn("Unexpected cast: ~p", [Unexpected]),
    {noreply, S0}.



handle_info(?do_beat(), S0) ->
    handle_info_do_beat(S0);

handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec handle_info_do_beat(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_do_beat(#state{
    tx_pid = TxPid,
    heartbeat_interval = HeartbeatInterval,
    last_seq = LastSeq
} = S0) ->
    {ok, Event} = marvin_pdu:render(marvin_pdu_heartbeat:new(LastSeq)),
    marvin_log:debug("Shard '~p' now heartbeats", [S0#state.shard_name]),
    ok = marvin_shard_tx:send_sync(TxPid, Event),
    {ok, TRef} = schedule_beat(HeartbeatInterval),
    {noreply, S0#state{
        timer_reference = TRef,
        waiting_for_ack = true
    }}.



-spec handle_call_heartbeat_ack(State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_heartbeat_ack(S0) ->
    marvin_log:debug("Shard '~p' got heartbeat ack", [S0#state.shard_name]),
    {reply, ok, S0#state{
        waiting_for_ack = false
    }}.



-spec schedule_beat(After :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: timer:tref()).

schedule_beat(After) ->
    {ok, _Tref} = timer:send_after(After, ?do_beat()).
