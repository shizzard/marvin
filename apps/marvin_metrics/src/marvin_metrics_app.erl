-module(marvin_metrics_app).
-behaviour(application).
-include_lib("marvin_helper/include/marvin_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/metrics", marvin_metrics_prometheus_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(
        prometheus_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    marvin_metrics_sup:start_link().

stop(_State) ->
    ok.
