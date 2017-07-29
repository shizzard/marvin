-module(marvin_shard_sup).
-behaviour(supervisor).
-include_lib("marvin_helper/include/marvin_specs_supervisor.hrl").

-export([start_link/0, init/1]).
-export([
    start_shard_session/2, stop_shard_session/1,
    start_shard_rx/3, stop_shard_rx/1,
    start_shard_tx/3, stop_shard_tx/1,
    start_shard_heart/3, stop_shard_heart/1
]).



%% Interface



-spec start_shard_session(ShardId :: non_neg_integer(), WssUrl :: binary()) ->
    marvin_helper_type:ok_return(OkRet :: pid() | undefined) |
    marvin_helper_type:ok_return(OkRet1 :: pid() | undefined, OkRet2 :: term()) |
    marvin_helper_type:error_return(ErrorRet :: already_present | {already_started, Child :: pid() | undefined} | term()).

start_shard_session(ShardId, WssUrl) ->
    ChildSpec = {shard_name(ShardId, session), {
        marvin_shard_session, start_link, [ShardId, shard_name(ShardId, session), WssUrl]
    }, permanent, 15000, worker, [marvin_shard_session]},
    supervisor:start_child(?MODULE, ChildSpec).



-spec start_shard_rx(ShardId :: non_neg_integer(), SessionPid :: pid(), WssUrl :: binary()) ->
    marvin_helper_type:ok_return(OkRet :: pid() | undefined) |
    marvin_helper_type:ok_return(OkRet1 :: pid() | undefined, OkRet2 :: term()) |
    marvin_helper_type:error_return(ErrorRet :: already_present | {already_started, Child :: pid() | undefined} | term()).

start_shard_rx(ShardId, SessionPid, WssUrl) ->
    ChildSpec = {shard_name(ShardId, rx), {
        marvin_shard_rx, start_link, [ShardId, shard_name(ShardId, rx), SessionPid, WssUrl]
    }, temporary, 15000, worker, [marvin_shard_rx]},
    supervisor:start_child(?MODULE, ChildSpec).



-spec start_shard_tx(ShardId :: non_neg_integer(), RxPid :: pid(), WssPid :: pid()) ->
    marvin_helper_type:ok_return(OkRet :: pid() | undefined) |
    marvin_helper_type:ok_return(OkRet1 :: pid() | undefined, OkRet2 :: term()) |
    marvin_helper_type:error_return(ErrorRet :: already_present | {already_started, Child :: pid() | undefined} | term()).

start_shard_tx(ShardId, RxPid, WssPid) ->
    ChildSpec = {shard_name(ShardId, tx), {
        marvin_shard_tx, start_link, [ShardId, shard_name(ShardId, tx), RxPid, WssPid]
    }, temporary, 5000, worker, [marvin_shard_tx]},
    supervisor:start_child(?MODULE, ChildSpec).



-spec start_shard_heart(ShardId :: non_neg_integer(), TxPid :: pid(), HeartbeatInterval :: non_neg_integer()) ->
    marvin_helper_type:ok_return(OkRet :: pid() | undefined) |
    marvin_helper_type:ok_return(OkRet1 :: pid() | undefined, OkRet2 :: term()) |
    marvin_helper_type:error_return(ErrorRet :: already_present | {already_started, Child :: pid() | undefined} | term()).

start_shard_heart(ShardId, TxPid, HeartbeatInterval) ->
    ChildSpec = {shard_name(ShardId, heart), {
        marvin_shard_heart, start_link, [ShardId, shard_name(ShardId, heart), TxPid, HeartbeatInterval]
    }, temporary, 5000, worker, [marvin_shard_heart]},
    supervisor:start_child(?MODULE, ChildSpec).



-spec stop_shard_session(ShardId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_shard_session(ShardId) ->
    supervisor:terminate_child(?MODULE, shard_name(ShardId, session)),
    supervisor:delete_child(?MODULE, shard_name(ShardId, session)),
    ok.



-spec stop_shard_rx(ShardId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_shard_rx(ShardId) ->
    supervisor:terminate_child(?MODULE, shard_name(ShardId, rx)),
    supervisor:delete_child(?MODULE, shard_name(ShardId, rx)),
    ok.



-spec stop_shard_tx(ShardId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_shard_tx(ShardId) ->
    supervisor:terminate_child(?MODULE, shard_name(ShardId, tx)),
    supervisor:delete_child(?MODULE, shard_name(ShardId, tx)),
    ok.



-spec stop_shard_heart(ShardId :: non_neg_integer()) ->
    marvin_helper_type:ok_return().

stop_shard_heart(ShardId) ->
    supervisor:terminate_child(?MODULE, shard_name(ShardId, heart)),
    supervisor:delete_child(?MODULE, shard_name(ShardId, heart)),
    ok.



-spec start_link() ->
    marvin_helper_type:generic_return(
        OkRet :: pid(),
        ErrorRet :: {already_started, pid()} | {shutdown, term()} | term()
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.



%% Internals



-spec shard_name(Id :: non_neg_integer(), Type :: session | rx | tx | heart) ->
    ShardName :: atom().

shard_name(0, session) -> 'marvin_shard_session_00';
shard_name(1, session) -> 'marvin_shard_session_01';
shard_name(2, session) -> 'marvin_shard_session_02';
shard_name(3, session) -> 'marvin_shard_session_03';
shard_name(4, session) -> 'marvin_shard_session_04';
shard_name(5, session) -> 'marvin_shard_session_05';
shard_name(6, session) -> 'marvin_shard_session_06';
shard_name(7, session) -> 'marvin_shard_session_07';
shard_name(8, session) -> 'marvin_shard_session_08';
shard_name(9, session) -> 'marvin_shard_session_09';
shard_name(10, session) -> 'marvin_shard_session_10';
shard_name(11, session) -> 'marvin_shard_session_11';
shard_name(12, session) -> 'marvin_shard_session_12';
shard_name(13, session) -> 'marvin_shard_session_13';
shard_name(14, session) -> 'marvin_shard_session_14';
shard_name(15, session) -> 'marvin_shard_session_15';

shard_name(0, rx) -> 'marvin_shard_rx_00';
shard_name(1, rx) -> 'marvin_shard_rx_01';
shard_name(2, rx) -> 'marvin_shard_rx_02';
shard_name(3, rx) -> 'marvin_shard_rx_03';
shard_name(4, rx) -> 'marvin_shard_rx_04';
shard_name(5, rx) -> 'marvin_shard_rx_05';
shard_name(6, rx) -> 'marvin_shard_rx_06';
shard_name(7, rx) -> 'marvin_shard_rx_07';
shard_name(8, rx) -> 'marvin_shard_rx_08';
shard_name(9, rx) -> 'marvin_shard_rx_09';
shard_name(10, rx) -> 'marvin_shard_rx_10';
shard_name(11, rx) -> 'marvin_shard_rx_11';
shard_name(12, rx) -> 'marvin_shard_rx_12';
shard_name(13, rx) -> 'marvin_shard_rx_13';
shard_name(14, rx) -> 'marvin_shard_rx_14';
shard_name(15, rx) -> 'marvin_shard_rx_15';

shard_name(0, tx) -> 'marvin_shard_tx_00';
shard_name(1, tx) -> 'marvin_shard_tx_01';
shard_name(2, tx) -> 'marvin_shard_tx_02';
shard_name(3, tx) -> 'marvin_shard_tx_03';
shard_name(4, tx) -> 'marvin_shard_tx_04';
shard_name(5, tx) -> 'marvin_shard_tx_05';
shard_name(6, tx) -> 'marvin_shard_tx_06';
shard_name(7, tx) -> 'marvin_shard_tx_07';
shard_name(8, tx) -> 'marvin_shard_tx_08';
shard_name(9, tx) -> 'marvin_shard_tx_09';
shard_name(10, tx) -> 'marvin_shard_tx_10';
shard_name(11, tx) -> 'marvin_shard_tx_11';
shard_name(12, tx) -> 'marvin_shard_tx_12';
shard_name(13, tx) -> 'marvin_shard_tx_13';
shard_name(14, tx) -> 'marvin_shard_tx_14';
shard_name(15, tx) -> 'marvin_shard_tx_15';

shard_name(0, heart) -> 'marvin_shard_heart_00';
shard_name(1, heart) -> 'marvin_shard_heart_01';
shard_name(2, heart) -> 'marvin_shard_heart_02';
shard_name(3, heart) -> 'marvin_shard_heart_03';
shard_name(4, heart) -> 'marvin_shard_heart_04';
shard_name(5, heart) -> 'marvin_shard_heart_05';
shard_name(6, heart) -> 'marvin_shard_heart_06';
shard_name(7, heart) -> 'marvin_shard_heart_07';
shard_name(8, heart) -> 'marvin_shard_heart_08';
shard_name(9, heart) -> 'marvin_shard_heart_09';
shard_name(10, heart) -> 'marvin_shard_heart_10';
shard_name(11, heart) -> 'marvin_shard_heart_11';
shard_name(12, heart) -> 'marvin_shard_heart_12';
shard_name(13, heart) -> 'marvin_shard_heart_13';
shard_name(14, heart) -> 'marvin_shard_heart_14';
shard_name(15, heart) -> 'marvin_shard_heart_15';

shard_name(Id, Type) -> error({not_implemented, {Id, Type}}).
