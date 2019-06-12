-module(marvin_rest2_impl_message_create).
-behavior(marvin_rest2_request).

-include("marvin_rest2.hrl").

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> ?ratelimit_group_channel.

method() -> post.

pdu() -> marvin_pdu2_rest_message_create.

url_template() -> <<"/channels/{{channel_id}}/messages">>.
