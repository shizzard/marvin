-module(marvin_guild_helper_role).

-include("marvin_guild_state.hrl").

-export([handle_call_do_provision_chain/1]).



%% Interface



-spec handle_call_do_provision_chain({Struct :: marvin_pdu2_dispatch_guild_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_guild_create:t(),
        S1 :: state()
    }).

handle_call_do_provision_chain({Struct, S0}) ->
    Roles = marvin_pdu2_dispatch_guild_create:roles(Struct),
    marvin_log:info("Guild '~s' roles: ~p total", [S0#state.guild_id, length(Roles)]),
    S1 = lists:foldl(fun set_role/2, S0, Roles),
    {ok, {Struct, S1}}.



%% Internals



-spec set_role(RoleStruct :: marvin_pdu2_object_role:t(), S0 :: state()) ->
    Ret :: state().

set_role(RoleStruct, #state{role_state = RoleStateEts} = S0) ->
    ets:insert(RoleStateEts, #role{
        role_id = marvin_pdu2_object_role:id(RoleStruct),
        role = RoleStruct
    }),
    S0.
