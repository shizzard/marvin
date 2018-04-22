-module(marvin_plugin_config).
-compile({parse_transform, cloak_transform}).

-export([export/1]).

-record(?MODULE, {
    plugin_id :: atom()
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



%% Interface



export(#?MODULE{plugin_id = PluginId}) ->
    #{<<"plugin_id">> => PluginId}.



%% Internals
