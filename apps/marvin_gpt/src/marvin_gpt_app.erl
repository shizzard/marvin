-module(marvin_gpt_app).
-behaviour(application).
-include_lib("marvin_helper/include/marvin_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    hackney_pool:start_pool(marvin_gpt, [{timeout, 10000}, {max_connections, 20}]),
    marvin_gpt_sup:start_link().

stop(_State) ->
    ok.
