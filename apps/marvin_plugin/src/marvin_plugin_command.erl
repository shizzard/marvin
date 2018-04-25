-module(marvin_plugin_command).
-compile({parse_transform, cloak_transform}).

-export([short/1]).

-record(?MODULE, {
    plugin_id :: marvin_plugin:id(),
    command :: binary(),
    help :: binary(),
    keywords :: [binary()]
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).



short(#?MODULE{plugin_id = PluginId, command = Command}) ->
    <<PluginId/binary, ":", Command/binary>>.
