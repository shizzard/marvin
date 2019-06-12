-module(marvin_rest2_impl_guild_member_role_add).
-behavior(marvin_rest2_request).

-include("marvin_rest2.hrl").

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> ?ratelimit_group_guild.

method() -> put.

pdu() -> undefined.

url_template() -> <<"/guilds/{{guild_id}}/members/{{user_id}}/roles/{{role_id}}">>.
