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
        {name, marvin_gateway_meta_http_request_duration_us},
        {help, "Gateway metadata request duration in microseconds"},
        {buckets, prometheus_buckets:new({linear, 100000, 50000, 10})}
    ]),
    prometheus_gauge:new([
        {name, marvin_gateway_meta_http_request_interval_s},
        {help, "Gateway metadata requests interval in seconds"}
    ]),
    prometheus_gauge:new([
        {name, marvin_gateway_meta_shards_count_i},
        {help, "Amount of shards started"}
    ]).
