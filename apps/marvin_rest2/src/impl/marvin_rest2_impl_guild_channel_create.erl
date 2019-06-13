-module(marvin_rest2_impl_guild_channel_create).
-behavior(marvin_rest2_request).

-include("marvin_rest2.hrl").

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> ?ratelimit_group_guild.

method() -> post.

pdu() -> marvin_pdu2_rest_guild_channel_create.

url_template() -> <<"/guilds/{{guild_id}}/channels">>.
