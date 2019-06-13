-module(marvin_rest2_app).
-behaviour(application).

-include("marvin_rest2.hrl").
-include_lib("marvin_helper/include/marvin_specs_application.hrl").

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    [
        hackney_pool:start_pool(RateLimitGroup, [{timeout, 10000}, {max_connections, 20}])
        || RateLimitGroup <- [?ratelimit_group_guild, ?ratelimit_group_channel, ?ratelimit_group_webhook]
    ],
    marvin_rest2_sup:start_link().

stop(_State) ->
    ok.
