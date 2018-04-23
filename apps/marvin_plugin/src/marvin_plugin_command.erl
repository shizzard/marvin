-module(marvin_plugin_command).
-compile({parse_transform, cloak_transform}).

-record(?MODULE, {
    plugin_id :: marvin_plugin:id(),
    command :: binary(),
    help :: binary(),
    keywords :: [binary()]
}).
-type t() :: #?MODULE{}.
-export_type([t/0]).
