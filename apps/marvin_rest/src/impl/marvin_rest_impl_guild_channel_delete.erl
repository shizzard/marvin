-module(marvin_rest_impl_guild_channel_delete).
-behavior(marvin_rest_request).

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> '/channels/{channel_id}'.

method() -> delete.

pdu() -> undefined.

url_template() -> <<"/channels/{{channel_id}}">>.
