-module(marvin_rest_impl_message_create).
-behavior(marvin_rest_request).

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> channel_id.

method() -> post.

pdu() -> marvin_pdu2_rest_message_create.

url_template() -> <<"/channels/{{channel_id}}/messages">>.
