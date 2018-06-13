-module(marvin_rest_impl_guild_member_role_delete).
-behavior(marvin_rest_request).

-export([ratelimit_group/0, method/0, pdu/0, url_template/0]).


ratelimit_group() -> guild_id.

method() -> delete.

pdu() -> undefined.

url_template() -> <<"/guilds/{{guild_id}}/members/{{user_id}}/roles/{{role_id}}">>.
