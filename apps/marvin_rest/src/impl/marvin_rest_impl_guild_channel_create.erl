-module(marvin_rest_impl_guild_channel_create).
-behavior(marvin_rest_request).

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> guild_id.

method() -> post.

pdu() -> marvin_pdu2_rest_guild_channel_create.

url_template() -> <<"/guilds/{{guild_id}}/channels">>.
