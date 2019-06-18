-module(marvin_plugin_lfg_handler_member_update).

-include("marvin_plugin_lfg.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([handle_member_update/2, handle_presence_update/2, maybe_change_role_active_state/4]).



handle_member_update(Event, S0) ->
    #{
        <<"role_active">> := RoleActive
    } = marvin_plugin_config:data(S0#state.config),
    Member = marvin_guild_helper_members:r_get_member_by_user_id(
        marvin_pdu2_object_user:id(
            marvin_pdu2_dispatch_guild_member_update:user(
                marvin_guild_pubsub:payload(Event))),
        marvin_guild_pubsub:guild_context(Event)
    ),
    maybe_change_role_active_state(Member, Event, RoleActive, S0#state.game_roles),
    {noreply, S0}.



handle_presence_update(Event, S0) ->
    #{
        <<"role_active">> := RoleActive
    } = marvin_plugin_config:data(S0#state.config),
    Member = marvin_guild_helper_members:r_get_member_by_user_id(
        marvin_pdu2_dispatch_presence_update:user(
            marvin_guild_pubsub:payload(Event)),
        marvin_guild_pubsub:guild_context(Event)
    ),
    maybe_change_role_active_state(Member, Event, RoleActive, S0#state.game_roles),
    {noreply, S0}.



maybe_change_role_active_state(Member, Event, RoleActive, GameRoles) ->
    HasGameRoles = lists:any(
        fun(MemberRole) -> lists:member(MemberRole, GameRoles) end,
        marvin_pdu2_object_member:roles(Member)
    ),
    IsOnline = <<"online">> == marvin_guild_helper_presence:r_get_member_status(
        marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
        marvin_guild_pubsub:guild_context(Event)
    ),
    HasRoleActive = lists:member(RoleActive, marvin_pdu2_object_member:roles(Member)),
    case {HasGameRoles, HasRoleActive, IsOnline} of
        {HasGameRoles, true, IsOnline} when (not HasGameRoles) orelse (not IsOnline) ->
            ?l_alert(#{
                text => "Plugin removing role-active for user",
                what => handle_info,
                details => #{
                    guild_id => marvin_guild_context:guild_id(
                        marvin_guild_pubsub:guild_context(Event)),
                    has_game_roles => HasGameRoles, has_role_active => HasRoleActive, is_online => IsOnline,
                    member_nick => marvin_pdu2_object_member:nick(Member),
                    event => marvin_guild_pubsub:payload(Event)
                }
            }),
            marvin_rest2:enqueue_request(marvin_rest2_request:new(
                marvin_rest2_impl_guild_member_role_delete,
                #{
                    <<"guild_id">> => marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event)),
                    <<"user_id">> => marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
                    <<"role_id">> => RoleActive
                }, #{}
            ));
        %% Member has game roles, does not have role-active
        {true, false, true} ->
            ?l_alert(#{
                text => "Plugin adding role-active for user",
                what => handle_info,
                details => #{
                    guild_id => marvin_guild_context:guild_id(
                        marvin_guild_pubsub:guild_context(Event)),
                    has_game_roles => HasGameRoles, has_role_active => HasRoleActive, is_online => IsOnline,
                    member_nick => marvin_pdu2_object_member:nick(Member),
                    event => marvin_guild_pubsub:payload(Event)
                }
            }),
            marvin_rest2:enqueue_request(marvin_rest2_request:new(
                marvin_rest2_impl_guild_member_role_add,
                #{
                    <<"guild_id">> => marvin_guild_context:guild_id(marvin_guild_pubsub:guild_context(Event)),
                    <<"user_id">> => marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
                    <<"role_id">> => RoleActive
                }, #{}
            ));
        %% Do not do anything
        {_, _, _} -> none
    end,
    ok.


