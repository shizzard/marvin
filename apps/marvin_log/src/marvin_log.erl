-module(marvin_log).
-marvin_mod(marvin_log).
-include_lib("kernel/include/logger.hrl").

-export([log_map/1, log_meta/1, target/2, http_req_map/1, http_ret_map/1]).

-define(default_log, #{
    what => 'NONE',
    text => 'NONE',
    result => 'NONE',
    details => 'NONE'
}).

-define(default_meta, #{
    id => uuid:uuid_to_string(uuid:get_v4_urandom()),
    in => 'NONE'
}).

-define(default_http_req, #{
    method => 'NONE',
    path => 'NONE',
    size => 'NONE'
}).

-define(default_http_ret, #{
    code => 'NONE',
    path => 'NONE'
}).



%% Interface



-spec log_map(InMap :: map()) ->
    Ret :: map().

log_map(InMap) ->
    maps:merge(?default_log, InMap).



-spec log_meta(InMap :: map()) ->
    Ret :: map().

log_meta(InMap) ->
    maps:merge(?default_meta, InMap).



-spec target(Host :: binary(), Port :: pos_integer()) ->
    Ret :: map().

target(Host, Port) ->
    #{host => Host, port => Port}.



-spec http_ret_map(InMap :: map()) ->
    Ret :: map().

http_ret_map(InMap) ->
    maps:merge(?default_http_ret, InMap).



-spec http_req_map(InMap :: map()) ->
    Ret :: map().

http_req_map(InMap) ->
    maps:merge(?default_http_req, InMap).
