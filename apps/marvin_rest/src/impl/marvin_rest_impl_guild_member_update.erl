-module(marvin_rest_impl_guild_member_update).
-behavior(marvin_rest_request).

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> '/guilds/{guild.id}/members/{user.id}'.

method() -> patch.

pdu() -> marvin_pdu2_rest_guild_member_update.

url_template() -> <<"/guilds/{{guild_id}}/members/{{user_id}}">>.
