-module(marvin_app).
-behaviour(application).
-include_lib("marvin_helper/include/marvin_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    declare_metrics(),
    marvin_sup:start_link().

stop(_State) ->
    ok.


declare_metrics() ->
    application:ensure_started(prometheus),
    %% marvin_gateway_meta
    prometheus_histogram:new([
        {name, marvin_gateway_http_request_duration_seconds},
        {help, "Gateway metadata request duration in seconds"},
        {buckets, prometheus_buckets:new({linear, 0.05, 0.05, 20})},
        {labels, [kind]}
    ]),
    prometheus_gauge:new([
        {name, marvin_gateway_http_request_interval_seconds},
        {help, "Gateway metadata requests interval in seconds"},
        {duration_unit, false}
    ]),
    prometheus_gauge:new([
        {name, marvin_gateway_shards_count_items},
        {help, "Amount of shards started"}
    ]),
    %% marvin_guild
    prometheus_gauge:new([
        {name, marvin_guild_presence_state},
        {help, "Amount of users per guild"},
        {labels, [guild_id, status]}
    ]).
