-module(marvin_plugin_lfg_handler_role_change).

-include("marvin_plugin_lfg.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([handle/2]).



handle(Event, S0) ->
    #{
        <<"role_active">> := RoleActive
    } = marvin_plugin_config:data(S0#state.config),
    ActionCreate = marvin_guild_pubsub:action_create(),
    ActionUpdate = marvin_guild_pubsub:action_update(),
    ActionDelete = marvin_guild_pubsub:action_delete(),
    S1 = recalculate_game_roles(marvin_guild_pubsub:guild_context(Event), S0),
    case marvin_guild_pubsub:action(Event) of
        ActionCreate ->
            %% On role create we don't have any members with this role,
            %% so the only thing we need to do is to refresh th e roles list
            ok;
        ActionUpdate ->
            %% We need to add role-active for the role users, and refresh the
            %% roles list
            MembersWithRole = marvin_guild_helper_members:r_get_members_by_role(
                marvin_pdu2_object_role:id(marvin_pdu2_dispatch_guild_role_update:role(marvin_guild_pubsub:payload(Event))),
                marvin_guild_pubsub:guild_context(Event)
            ),
            %% "Simulate" member update;
            %% WARNING! this code relies on the fact that the following function has NO side-effects
            %% `marvin_plugin_lfg_handler_member_update:maybe_change_role_active_state/4`
            [_ = marvin_plugin_lfg_handler_member_update:maybe_change_role_active_state(
                Member, Event, RoleActive, S1#state.game_roles
            ) || Member <- MembersWithRole],
            ok;
        ActionDelete ->
            %% We need to remove role-active for the role users for those of
            %% them that don't have another game roles, and refresh the roles
            %% list
            MembersWithRole = marvin_guild_helper_members:r_get_members_by_role(
                marvin_pdu2_dispatch_guild_role_delete:role_id(marvin_guild_pubsub:payload(Event)),
                marvin_guild_pubsub:guild_context(Event)
            ),
            %% "Simulate" member update;
            %% WARNING! this code relies on the fact that the following function has NO side-effects
            %% `marvin_plugin_lfg_handler_member_update:maybe_change_role_active_state/4`
            [_ = marvin_plugin_lfg_handler_member_update:maybe_change_role_active_state(
                Member, Event, RoleActive, S1#state.game_roles
            ) || Member <- MembersWithRole],
            ok
    end,
    {noreply, S1}.



recalculate_game_roles(GuildCtx, S0) ->
    #{
        <<"role_game_prefix">> := RoleGamePrefix
    } = marvin_plugin_config:data(S0#state.config),
    Roles = marvin_guild_helper_role:r_get_all_roles(GuildCtx),
    GameRoles = [
        marvin_pdu2_object_role:id(Role) || Role <- Roles,
        marvin_plugin_lfg:is_game_role_name(RoleGamePrefix, marvin_pdu2_object_role:name(Role))
    ],
    S0#state{game_roles = GameRoles}.
