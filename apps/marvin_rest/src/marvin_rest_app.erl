-module(marvin_rest_app).
-behaviour(application).
-include_lib("marvin_helper/include/marvin_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    [
        hackney_pool:start_pool(RateLimitGroup, [{timeout, 10000}, {max_connections, 20}])
        || RateLimitGroup <- [guild_id, channel_id, webhook_id]
    ],
    marvin_rest_sup:start_link().

stop(_State) ->
    ok.
