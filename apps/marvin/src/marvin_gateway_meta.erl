-module(marvin_gateway_meta).
-behaviour(gen_server).

-include_lib("marvin_helper/include/marvin_specs_gen_server.hrl").

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-type internal_state() :: initial | gun_connect | gun_up | gun_wait | maybe_rebuild_layout.


-define(gun_connect(), {gun_connect}).
-define(gun_up(ConnPid, Proto), {gun_up, ConnPid, Proto}).
-define(gun_response(ConnPid, StreamRef, Type, HttpCode, Data),
    {gun_response, ConnPid, StreamRef, Type, HttpCode, Data}).
-define(gun_data(ConnPid, StreamRef, Type, Data),
    {gun_data, ConnPid, StreamRef, Type, Data}).
-define(gather_meta(), {gather_meta}).
-define(maybe_rebuild_layout(), {maybe_rebuild_layout}).

-record(state, {
    internal_state :: internal_state(),
    gun_pid :: pid() | undefined,
    gun_stream_reference :: reference() | undefined,
    last_time_gather :: non_neg_integer(),
    timer_reference :: timer:tref() | undefined,
    shards_count :: non_neg_integer(),
    running_shards_count :: non_neg_integer(),
    wss_url :: binary() | undefined
}).
-type state() :: #state{}.



%% Interface



-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init(_) ->
    self() ! ?gun_connect(),
    {ok, #state{
        internal_state = initial,
        last_time_gather = 0,
        shards_count = 0,
        running_shards_count = 0
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

handle_info(?gather_meta(), S0) ->
    handle_info_gather_meta(S0);

handle_info(?gun_response(_ConnPid, StreamRef, Type, HttpCode, Data), S0) ->
    handle_info_gun_response(StreamRef, Type, HttpCode, Data, S0);

handle_info(?gun_data(_ConnPid, StreamRef, Type, Data), S0) ->
    handle_info_gun_data(StreamRef, Type, Data, S0);

handle_info(?maybe_rebuild_layout(), S0) ->
    handle_info_maybe_rebuild_layout(S0);

handle_info(Unexpected, S0) ->
    marvin_log:warn("Unexpected info: ~p", [Unexpected]),
    {noreply, S0}.



terminate(_Reason, _S0) ->
    ok.



code_change(_OldVsn, S0, _Extra) ->
    {ok, S0}.



%% Internals



-spec handle_info_gun_connect(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_gun_connect(#state{internal_state = initial} = S0) ->
    {ok, ApiHost} = marvin_config:get(marvin, [discord, api, host]),
    {ok, ApiPort} = marvin_config:get(marvin, [discord, api, port]),
    marvin_log:info("Connecting to '~s:~p' to gather meta", [ApiHost, ApiPort]),
    {ok, GunPid} = gun:open(ApiHost, ApiPort),
    {noreply, S0#state{
        internal_state = gun_connect,
        gun_pid = GunPid
    }};

handle_info_gun_connect(#state{internal_state = InternalState} = S0) ->
    marvin_log:error(
        "Got 'gun_connect' message while in internal_state ~p, state ~p, recovering to initial",
        [InternalState, S0]
    ),
    recover_state_to_initial(S0).



-spec handle_info_gun_up(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_gun_up(#state{internal_state = gun_connect} = S0) ->
    marvin_log:debug("Connection up, scheduling meta gathering", []),
    {ok, TRef} = set_gather_meta_timer(0),
    {noreply, S0#state{
        internal_state = gun_up,
        timer_reference = TRef
    }};

handle_info_gun_up(#state{internal_state = InternalState} = S0) ->
    marvin_log:error(
        "Got 'gun_up' message while in internal_state ~p, state ~p, recovering to initial",
        [InternalState, S0]
    ),
    recover_state_to_initial(S0).




-spec handle_info_gather_meta(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_gather_meta(#state{internal_state = gun_up} = S0) ->
    UserAgent = get_header_user_agent(),
    Authorization = get_header_authorization(),
    marvin_log:debug("Gathering meta information with user-agent ~p", [UserAgent]),
    StreamRef = gun:get(S0#state.gun_pid, get_gateway_url(), [
        {<<"user-agent">>, UserAgent},
        {<<"authorization">>, Authorization}
    ]),
    {noreply, S0#state{
        internal_state = gun_wait,
        gun_stream_reference = StreamRef
    }};

handle_info_gather_meta(#state{internal_state = InternalState} = S0) ->
    marvin_log:error(
        "Got 'gather_meta' message while in internal_state ~p, state ~p, recovering to initial",
        [InternalState, S0]
    ),
    recover_state_to_initial(S0).



-spec handle_info_gun_response(
    StreamRef :: reference(),
    Type :: nofin | fin,
    HttpCode :: non_neg_integer(),
    Data :: marvin_helper_type:proplist(KeyT :: binary(), ValueT :: term()) | binary(),
    State :: state()
) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_gun_response(StreamRef, nofin, 200, Data, #state{
    internal_state = gun_wait,
    gun_stream_reference = StreamRef
} = S0) when is_list(Data) ->
    case proplists:get_value(<<"x-ratelimit-reset">>, Data, undefined) of
        undefined ->
            marvin_log:error(
                "Error while gathering meta: no 'x-ratelimit-reset' header found, falling to +10s",
                []
            ),
            {ok, TRef} = set_gather_meta_timer(10),
            {noreply, S0#state{
                last_time_gather = marvin_helper_time:timestamp(),
                timer_reference = TRef
            }};
        TimestampBin ->
            FutureTimestamp = binary_to_integer(TimestampBin),
            CurrentTimestamp = marvin_helper_time:timestamp(),
            IntervalSec = (FutureTimestamp - CurrentTimestamp) * 2,
            {ok, TRef} = set_gather_meta_timer(IntervalSec),
            {noreply, S0#state{
                last_time_gather = marvin_helper_time:timestamp(),
                timer_reference = TRef
            }}
    end;

handle_info_gun_response(_StreamRef, _Type, _HttpCode, _Data, #state{
    internal_state = InternalState
} = S0) ->
    marvin_log:error(
        "Got 'gun_response' message while in internal_state ~p, state ~p, recovering to initial",
        [InternalState, S0]
    ),
    recover_state_to_initial(S0).



-spec handle_info_gun_data(
    StreamRef :: reference(),
    Type :: nofin | fin,
    Data :: binary(),
    State :: state()
) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_gun_data(StreamRef, nofin, Data, #state{
    internal_state = gun_wait,
    gun_stream_reference = StreamRef
} = S0) when is_binary(Data) ->
    try
        #{<<"url">> := WssUrl, <<"shards">> := ShardsCount} = jiffy:decode(Data, [return_maps]),
        {noreply, S0#state{
            wss_url = WssUrl,
            shards_count = ShardsCount
        }}
    catch Type:Error ->
        marvin_log:error(
            "Failed to decode gathered meta with ~p:~p when data was ~p, ignoring",
            [Type, Error, Data]
        ),
        {noreply, S0}
    end;

handle_info_gun_data(StreamRef, fin, _Data, #state{
    internal_state = gun_wait,
    gun_stream_reference = StreamRef
} = S0) ->
    self() ! ?maybe_rebuild_layout(),
    {noreply, S0#state{
        internal_state = maybe_rebuild_layout,
        gun_stream_reference = undefined
    }};

handle_info_gun_data(_StreamRef, _Type, Data, #state{
    internal_state = InternalState
} = S0) ->
    marvin_log:error(
        "Got 'gun_data' message with data ~p while in internal_state ~p, state ~p, recovering to initial",
        [Data, InternalState, S0]
    ),
    recover_state_to_initial(S0).



-spec handle_info_maybe_rebuild_layout(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

handle_info_maybe_rebuild_layout(#state{
    shards_count = ShardsCount,
    running_shards_count = RunningShardsCount
} = S0) when ShardsCount < RunningShardsCount ->
    marvin_log:info(
        "Going to reduce shards count due to switch ~p -> ~p",
        [RunningShardsCount, ShardsCount]
    ),
    ShardsToStop = lists:nthtail(ShardsCount, lists:seq(0, RunningShardsCount - 1)),
    lists:foreach(fun marvin_shard_sup:stop_shard_rx/1, ShardsToStop),
    {noreply, S0#state{
        internal_state = gun_up,
        running_shards_count = ShardsCount
    }};

handle_info_maybe_rebuild_layout(#state{
    shards_count = ShardsCount,
    running_shards_count = RunningShardsCount,
    wss_url = WssUrl
} = S0) when ShardsCount > RunningShardsCount ->
    marvin_log:info(
        "Going to increase shards count due to switch ~p -> ~p",
        [RunningShardsCount, ShardsCount]
    ),
    ShardsToStart = lists:nthtail(RunningShardsCount, lists:seq(0, ShardsCount - 1)),
    lists:foreach(fun(ShardId) ->
        marvin_shard_sup:start_shard_session(ShardId, WssUrl)
    end, ShardsToStart),
    {noreply, S0#state{
        internal_state = gun_up,
        running_shards_count = ShardsCount
    }};

handle_info_maybe_rebuild_layout(S0) ->
    {noreply, S0#state{
        internal_state = gun_up
    }}.



-spec recover_state_to_initial(State :: state()) ->
    marvin_helper_type:gen_server_noreply(State :: state()).

recover_state_to_initial(S0) ->
    marvin_log:info("Recovering to initial state", []),
    case S0#state.gun_pid of
        undefined -> ok;
        Pid ->
            marvin_log:info("Closing gun connection brutally", []),
            gun:close(Pid)
    end,
    case S0#state.timer_reference of
        undefined -> ok;
        TRef ->
            marvin_log:info("Cancelling timer", []),
            timer:cancel(TRef)
    end,
    marvin_log:info("Flushing message queue", []),
    flush_message_queue(),
    marvin_log:info("Sending 'gun_connect' message", []),
    self() ! ?gun_connect(),
    marvin_log:info("Resetting 'gun_stream_reference' and 'internal_state'", []),
    {noreply, #state{
        internal_state = initial,
        last_time_gather = S0#state.last_time_gather,
        shards_count = S0#state.shards_count,
        running_shards_count = S0#state.running_shards_count,
        wss_url = S0#state.wss_url
    }}.



-spec flush_message_queue() ->
    marvin_helper_type:ok_return().

flush_message_queue() ->
    receive
        _M -> flush_message_queue()
    after
        0 -> ok
    end.



-spec set_gather_meta_timer(IntervalSec :: non_neg_integer()) ->
    marvin_helper_type:generic_return(
        OkRet :: timer:tref(),
        ErrorRet :: term()
    ).

set_gather_meta_timer(IntervalSec) ->
    timer:send_after(IntervalSec * 1000, ?gather_meta()).



-spec get_gateway_url() ->
    Ret :: string().

get_gateway_url() ->
    {ok, ApiRootUrl} = marvin_config:get(marvin, [discord, api, root_url]),
    {ok, ApiGatewayUrl} = marvin_config:get(marvin, [discord, api, gateway_url]),
    ApiRootUrl ++ ApiGatewayUrl.



-spec get_header_user_agent() ->
    Ret :: binary().

get_header_user_agent() ->
    {ok, LibraryName} = marvin_config:get(marvin, [system_info, library_name]),
    {ok, LibraryVersion} = marvin_config:get(marvin, [system_info, library_version]),
    {ok, LibraryWeb} = marvin_config:get(marvin, [system_info, library_web]),
    iolist_to_binary([LibraryName, " (", LibraryWeb, ", v", LibraryVersion, ")"]).



-spec get_header_authorization() ->
    Ret :: binary().

get_header_authorization() ->
    {ok, Token} = marvin_config:get(marvin, [discord, token]),
    iolist_to_binary(["Bot ", Token]).
