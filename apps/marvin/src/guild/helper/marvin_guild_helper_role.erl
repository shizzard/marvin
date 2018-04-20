-module(marvin_guild_helper_role).

-include("marvin_guild_state.hrl").

-export([w_do_provision/2]).



%% Interface



-spec w_do_provision(Roles :: [marvin_pdu2_object_role:t()], Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_do_provision(Roles, Ctx) ->
    marvin_log:info("Guild '~s' roles: ~p total", [marvin_guild_context:guild_id(Ctx), length(Roles)]),
    ets:insert(marvin_guild_context:role_state(Ctx), [#role{
        role_id = marvin_pdu2_object_role:id(Role),
        role = Role
    } || Role <- Roles]),
    ok.
