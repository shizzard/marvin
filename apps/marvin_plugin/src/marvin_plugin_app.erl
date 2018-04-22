-module(marvin_plugin_app).
-behaviour(application).
-include_lib("marvin_helper/include/marvin_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    marvin_plugin_sup:start_link().

stop(_State) ->
    ok.
