-module(marvin_guild_helper_role).

-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([
    w_do_provision/2, w_role_create/2, w_role_update/2, w_role_delete/2,
    r_get_everyone/1, r_get_all_roles/1]).

-define(role_everyone, <<"@everyone">>).



%% Interface



-spec w_do_provision(Roles :: [marvin_pdu2_object_role:t()], Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_do_provision(Roles, Ctx) ->
    ?l_debug(#{
        text => "Guild roles provisioned",
        what => role_provision, result => ok,
        details => #{
            guild_id => marvin_guild_context:guild_id(Ctx),
            total => length(Roles)
        }
    }),
    ets:insert(marvin_guild_context:role_state(Ctx), [#role{
        role_id = marvin_pdu2_object_role:id(Role),
        role = Role
    } || Role <- Roles]),
    ok.



w_role_create(Struct, Ctx) ->
    Role = marvin_pdu2_dispatch_guild_role_create:role(Struct),
    ets:insert(marvin_guild_context:role_state(Ctx), #role{
        role_id = marvin_pdu2_object_role:id(Role),
        role = Role
    }),
    ok.



w_role_update(Struct, Ctx) ->
    Role = marvin_pdu2_dispatch_guild_role_update:role(Struct),
    ets:insert(marvin_guild_context:role_state(Ctx), #role{
        role_id = marvin_pdu2_object_role:id(Role),
        role = Role
    }),
    ok.



w_role_delete(Struct, Ctx) ->
    ets:delete(marvin_guild_context:role_state(Ctx), marvin_pdu2_dispatch_guild_role_delete:role_id(Struct)),
    ok.



-spec r_get_everyone(Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_pdu2_object_role:t().

r_get_everyone(Ctx) ->
    [Everyone] = [
        Role#role.role || Role <- ets:tab2list(marvin_guild_context:role_state(Ctx)),
        marvin_pdu2_object_role:name(Role#role.role) == ?role_everyone
    ],
    Everyone.



-spec r_get_all_roles(Ctx :: marvin_guild_context:t()) ->
    Ret :: [marvin_pdu2_object_role:t()].

r_get_all_roles(Ctx) ->
    [Role#role.role || Role <- ets:tab2list(marvin_guild_context:role_state(Ctx))].
