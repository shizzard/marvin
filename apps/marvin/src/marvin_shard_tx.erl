-module(marvin_shard_tx).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/3, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).



-record(state, {
    shard_id :: non_neg_integer(),
    shard_name :: atom(),
    wss_pid :: pid() | undefined
}).
-type state() :: #state{}.



%% Interface



-spec start_link(
    ShardId :: non_neg_integer(),
    ShardName :: atom(),
    WssPid :: pid()
) ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link(ShardId, ShardName, WssPid) ->
    gen_server:start_link({local, ShardName}, ?MODULE, [ShardId, ShardName, WssPid], []).



init([ShardId, ShardName, WssPid]) ->
    {ok, #state{
        shard_id = ShardId,
        shard_name = ShardName,
        wss_pid = WssPid
    }}.



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
