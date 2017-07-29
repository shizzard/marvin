-module(marvin_shard_tx).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/4, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).
-export([send_sync/2, send_async/2]).



-define(send_event(Event), {send_event, Event}).
-define(gun_fake_ws_send(RxPid, Event), {ws_send, RxPid, {text, Event}}).

-record(state, {
    shard_id :: non_neg_integer(),
    shard_name :: atom(),
    rx_pid :: pid(),
    wss_pid :: pid() | undefined
}).
-type state() :: #state{}.



%% Interface



-spec send_sync(TxPid :: pid(), Event :: binary()) ->
    marvin_helper_type:ok_return().

send_sync(TxPid, Event) ->
    gen_server:call(TxPid, ?send_event(Event)).



-spec send_async(TxPid :: pid(), Event :: binary()) ->
    marvin_helper_type:ok_return().

send_async(TxPid, Event) ->
    gen_server:cast(TxPid, ?send_event(Event)).



-spec start_link(
    ShardId :: non_neg_integer(),
    ShardName :: atom(),
    RxPid :: pid(),
    WssPid :: pid()
) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(ShardId, ShardName, RxPid, WssPid) ->
    gen_server:start_link({local, ShardName}, ?MODULE, [ShardId, ShardName, RxPid, WssPid], []).



init([ShardId, ShardName, RxPid, WssPid]) ->
    {ok, #state{
        shard_id = ShardId,
        shard_name = ShardName,
        rx_pid = RxPid,
        wss_pid = WssPid
    }}.



handle_call(?send_event(Event), _GenReplyTo, S0) ->
    handle_call_send_event(Event, S0);

handle_call(Unexpected, _GenReplyTo, S0) ->
    marvin_log:warn("Unexpected call: ~p", [Unexpected]),
    {reply, badarg, S0}.



handle_cast(?send_event(Event), S0) ->
    handle_cast_send_event(Event, S0);

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



-spec handle_call_send_event(Event :: binary(), State :: state()) ->
    marvin_helper_type:gen_server_reply_simple(
        Reply :: marvin_helper_type:ok_return(),
        State :: state()
    ).

handle_call_send_event(Event, #state{
    rx_pid = RxPid,
    wss_pid = WssPid
} = S0) ->
    WssPid ! ?gun_fake_ws_send(RxPid, Event),
    {reply, ok, S0}.



-spec handle_cast_send_event(Event :: binary(), State :: state()) ->
    marvin_helper_type:gen_server_noreply_simple(
        State :: state()
    ).

handle_cast_send_event(Event, #state{
    rx_pid = RxPid,
    wss_pid = WssPid
} = S0) ->
    WssPid ! ?gun_fake_ws_send(RxPid, Event),
    {noreply, S0}.
