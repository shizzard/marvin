-module(marvin_gateway_meta).
-behavior(gen_statem).
-include_lib("marvin_log/include/marvin_log.hrl").

-export([callback_mode/0, start_link/0, init/1, handle_event/4]).
-export([get_shards_count/0]).

-record(state, {
    api_host :: string(),
    api_port :: pos_integer(),
    api_root_url :: string(),
    api_gateway_url :: string(),
    header_user_agent :: string(),
    header_authorization :: string(),
    gun_pid :: pid() | undefined,
    gun_stream_reference :: reference() | undefined,
    http_req_start_time = erlang:monotonic_time() :: integer(),
    next_gather_after = 0 :: non_neg_integer(),
    shards_count = 0 :: non_neg_integer(),
    running_shards_count = 0 :: non_neg_integer(),
    wss_url :: binary() | undefined
}).

-define(gun_connect(), {gun_connect}).
-define(gun_up(ConnPid, Proto), {gun_up, ConnPid, Proto}).
-define(gather_meta(), {gather_meta}).
-define(gun_response(ConnPid, StreamRef, Type, HttpCode, Data),
    {gun_response, ConnPid, StreamRef, Type, HttpCode, Data}).
-define(gun_data(ConnPid, StreamRef, Type, Data),
    {gun_data, ConnPid, StreamRef, Type, Data}).
-define(maybe_rebuild_layout(), {maybe_rebuild_layout}).

-define(get_shards_count(), {get_shards_count}).


%% Interface


-spec get_shards_count() ->
    Ret :: non_neg_integer().

get_shards_count() ->
    gen_statem:call(?MODULE, ?get_shards_count()).


-spec start_link() ->
    marvin_helper_type:ok_return(OkRet :: pid()).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    do_report_shards_count(0),
    {ok, ApiHost} = marvin_config:get(marvin, [discord, api, host]),
    {ok, ApiPort} = marvin_config:get_integer(marvin, [discord, api, port]),
    {ok, ApiRootUrl} = marvin_config:get(marvin, [discord, api, root_url]),
    {ok, ApiGatewayUrl} = marvin_config:get(marvin, [discord, api, gateway_url]),
    {ok, on_start, #state{
        api_host = ApiHost,
        api_port = ApiPort,
        api_root_url = ApiRootUrl,
        api_gateway_url = ApiGatewayUrl,
        header_user_agent = get_header_user_agent(),
        header_authorization = get_header_authorization()
    }, {state_timeout, 0, ?gun_connect()}}.


callback_mode() ->
    handle_event_function.


%% States


handle_event(state_timeout, ?gun_connect(), on_start, S0) ->
    ?l_info(#{
        text => "Connecting to API gateway",
        what => connection, dst => marvin_log:target(S0#state.api_host, S0#state.api_port)
    }),
    {ok, GunPid} = gun:open(S0#state.api_host, S0#state.api_port),
    {next_state, on_gun_connect, S0#state{
        gun_pid = GunPid
    }};

handle_event(info, ?gun_up(_ConnPid, _Proto), on_gun_connect, S0) ->
    ?l_info(#{
        text => "Connected to API gateway",
        what => connection, result => ok, dst => marvin_log:target(S0#state.api_host, S0#state.api_port)
    }),
    {next_state, on_gun_up, S0, {state_timeout, 0, ?gather_meta()}};

handle_event(state_timeout, ?gather_meta(), on_gun_up, S0) ->
    ?l_debug(#{
        text => "Gathering API gateway meta",
        what => gather_meta, dst => marvin_log:target(S0#state.api_host, S0#state.api_port),
        http_req => marvin_log:http_req_map(#{
            path => S0#state.api_root_url ++ S0#state.api_gateway_url, agent => S0#state.header_user_agent
        })
    }),
    StreamRef = gun:get(S0#state.gun_pid, S0#state.api_root_url ++ S0#state.api_gateway_url, [
        {<<"user-agent">>, S0#state.header_user_agent},
        {<<"authorization">>, S0#state.header_authorization}
    ]),
    {next_state, on_gather_meta_headers, S0#state{
        http_req_start_time = erlang:monotonic_time(),
        gun_stream_reference = StreamRef
    }};

handle_event(info, ?gun_response(_ConnPid, StreamRef, nofin, 200, Data), on_gather_meta_headers, #state{
    gun_stream_reference = StreamRef,
    http_req_start_time = HttpReqStartTime
} = S0) when is_list(Data) ->
    do_report_http_req_duration(HttpReqStartTime),
    IntervalSec = case proplists:get_value(<<"x-ratelimit-reset">>, Data, undefined) of
        undefined ->
            ?l_warning(#{
                text => "No 'x-ratelimit-reset' header found, falling to 10 seconds",
                what => gather_meta_headers,
                dst => marvin_log:target(S0#state.api_host, S0#state.api_port)
            }),
            10;
        TimestampBin ->
            FutureTimestamp = binary_to_integer(TimestampBin),
            CurrentTimestamp = marvin_helper_time:timestamp(),
            case (FutureTimestamp - CurrentTimestamp) * 2 of
                CalculatedIntervalSec when CalculatedIntervalSec > 0 ->
                    CalculatedIntervalSec;
                _Negative ->
                    ?l_warning(#{
                        text => "'x-ratelimit-reset' header has invalid value, falling to 10 seconds",
                        what => gather_meta_headers,
                        dst => marvin_log:target(S0#state.api_host, S0#state.api_port),
                        details => #{
                            future_timestamp => FutureTimestamp,
                            current_timestamp => CurrentTimestamp,
                            original_header_value => TimestampBin
                        }
                    }),
                    10
            end
    end,
    do_report_http_request_interval(IntervalSec),
    ?l_debug(#{
        text => "Gathered API gateway meta headers",
        what => gather_meta_headers, result => ok, details => #{ratelimit_interval => IntervalSec},
        http_ret => marvin_log:http_ret_map(#{
            code => 200, path => S0#state.api_root_url ++ S0#state.api_gateway_url, agent => S0#state.header_user_agent
        })
    }),
    {next_state, on_gather_meta_body, S0#state{
        next_gather_after = IntervalSec * 1000
    }};

handle_event(info, ?gun_response(_ConnPid, _StreamRef, nofin, 401, _), on_gather_meta_headers, S0) ->
    ?l_critical(#{
        text => "Failed to gather API meta headers",
        what => gather_meta_headers, result => error, details => invalid_token,
        dst => marvin_log:target(S0#state.api_host, S0#state.api_port),
        http_ret => marvin_log:http_ret_map(#{
            code => 401, path => S0#state.api_root_url ++ S0#state.api_gateway_url, agent => S0#state.header_user_agent
        })
    }),
    {stop, invalid_token};

handle_event(info, ?gun_response(_ConnPid, _StreamRef, nofin, Code, Data), on_gather_meta_headers, S0) ->
    ?l_critical(#{
        text => "Failed to gather API meta headers",
        what => gather_meta_headers, result => error, details => Data,
        dst => marvin_log:target(S0#state.api_host, S0#state.api_port),
        http_ret => marvin_log:http_ret_map(#{
            code => Code, path => S0#state.api_root_url ++ S0#state.api_gateway_url, agent => S0#state.header_user_agent,
            authorization => S0#state.header_authorization
        })
    }),
    {stop, invalid_response};

handle_event(info, ?gun_data(_ConnPid, StreamRef, nofin, Data), on_gather_meta_body, #state{
    gun_stream_reference = StreamRef
} = S0) when is_binary(Data) ->
    try
        #{<<"url">> := WssUrl, <<"shards">> := ShardsCount} = jiffy:decode(Data, [return_maps]),
        ?l_debug(#{
            text => "Gathered API gateway meta body",
            what => gather_meta_body, result => ok, details => #{url => WssUrl, shards => ShardsCount}
        }),
        {next_state, on_gather_meta_fin, S0#state{
            wss_url = WssUrl,
            shards_count = ShardsCount
        }}
    catch Type:Error ->
        ?l_error(#{
            text => "Failed to gather API gateway meta body",
            what => gather_meta_body, result => Type, details => #{type => Type, error => Error, data => Data}
        }),
        {next_state, on_gather_meta_fin, S0}
    end;

handle_event(info, ?gun_data(_ConnPid, StreamRef, fin, _Data), on_gather_meta_fin, #state{
    gun_stream_reference = StreamRef
} = S0) ->
    {next_state, on_maybe_rebuild_layout, S0#state{
        gun_stream_reference = undefined
    }, {state_timeout, 0, ?maybe_rebuild_layout()}};

handle_event(state_timeout, ?maybe_rebuild_layout(), on_maybe_rebuild_layout, #state{
    shards_count = ShardsCount,
    running_shards_count = RunningShardsCount
} = S0) when ShardsCount < RunningShardsCount ->
    ?l_info(#{
        text => "Rebuilding layout",
        what => maybe_rebuild_layout, result => decrease, shards => #{old => RunningShardsCount, new => ShardsCount}
    }),
    ShardsToStop = lists:nthtail(ShardsCount, lists:seq(0, RunningShardsCount - 1)),
    lists:foreach(fun marvin_shard_sup:stop_shard_rx/1, ShardsToStop),
    do_report_shards_count(ShardsCount),
    {next_state, on_gun_up, S0#state{
        running_shards_count = ShardsCount
    }, {state_timeout, S0#state.next_gather_after, ?gather_meta()}};

handle_event(state_timeout, ?maybe_rebuild_layout(), on_maybe_rebuild_layout, #state{
    shards_count = ShardsCount,
    running_shards_count = RunningShardsCount,
    wss_url = WssUrl
} = S0) when ShardsCount > RunningShardsCount ->
    ?l_info(#{
        text => "Rebuilding layout",
        what => maybe_rebuild_layout, result => increase, shards => #{old => RunningShardsCount, new => ShardsCount}
    }),
    ShardsToStart = lists:nthtail(RunningShardsCount, lists:seq(0, ShardsCount - 1)),
    lists:foreach(fun(ShardId) ->
        marvin_shard_sup:start_shard_session(ShardId, WssUrl)
    end, ShardsToStart),
    do_report_shards_count(ShardsCount),
    {next_state, on_gun_up, S0#state{
        running_shards_count = ShardsCount
    }, {state_timeout, S0#state.next_gather_after, ?gather_meta()}};

handle_event(state_timeout, ?maybe_rebuild_layout(), on_maybe_rebuild_layout, S0) ->
    {next_state, on_gun_up, S0,
        {state_timeout, S0#state.next_gather_after, ?gather_meta()}
    };

handle_event({call, From}, ?get_shards_count(), _AnyState, S0) ->
    {keep_state, S0, {reply, From, S0#state.shards_count}}.


%% Internals


-spec do_report_http_req_duration(HttpReqStartTime :: erlang:timestamp()) ->
    marvin_helper_type:ok_return().

do_report_http_req_duration(HttpReqStartTime) ->
    prometheus_histogram:observe(
        marvin_gateway_http_request_duration_seconds,
        [meta],
        erlang:monotonic_time() - HttpReqStartTime
    ),
    ok.


-spec do_report_http_request_interval(IntervalSec :: pos_integer()) ->
    marvin_helper_type:ok_return().

do_report_http_request_interval(IntervalSec) ->
    prometheus_gauge:set(
        marvin_gateway_http_request_interval_seconds,
        IntervalSec
    ),
    ok.


-spec do_report_shards_count(ShardsCount :: pos_integer() | 0) ->
    marvin_helper_type:ok_return().

do_report_shards_count(ShardsCount) ->
    prometheus_gauge:set(
        marvin_gateway_shards_count_items,
        ShardsCount
    ),
    ok.



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
    iolist_to_binary(["Bot ", binary_to_list(Token)]).
