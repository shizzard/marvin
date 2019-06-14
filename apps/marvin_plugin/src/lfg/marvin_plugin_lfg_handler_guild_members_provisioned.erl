-module(marvin_plugin_lfg_handler_guild_members_provisioned).

-include("marvin_plugin_lfg.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([handle/2]).

-record(handle_info_guild_event_guild_members_provisioned, {
    role_active :: marvin_pdu2:snowflake(),
    role_game_prefix :: unicode:unicode_binary(),
    game_roles :: [marvin_pdu2:snowflake()],
    event :: #{},
    members_with_roles :: [marvin_pdu2:snowflake()]
}).



handle(Event, S0) ->
    %% Setting up the roles for members
    %% - Members with any game role (detected by role_game_prefix) and status
    %%   'online' should get the role_active
    %% - Members without any game role or with status other then 'online'
    %%   should not have the role_active
    #{
        <<"role_active">> := RoleActive,
        <<"role_game_prefix">> := RoleGamePrefix
    } = marvin_plugin_config:data(S0#state.config),
    marvin_helper_chain:chain(
        'marvin_plugin_lfg:handle_info_guild_event_guild_members_provisioned', [
            fun handle_info_guild_event_guild_members_provisioned_provision_context/1,
            fun handle_info_guild_event_guild_members_provisioned_detect_members/1
        ], #handle_info_guild_event_guild_members_provisioned{
            role_active = RoleActive,
            role_game_prefix = RoleGamePrefix,
            game_roles = S0#state.game_roles,
            event = Event
        }
    ),
    {noreply, S0}.



handle_info_guild_event_guild_members_provisioned_provision_context(Ctx) ->
    Members = marvin_guild_helper_members:r_get_members_by_roles(
        Ctx#handle_info_guild_event_guild_members_provisioned.game_roles,
        marvin_guild_pubsub:guild_context(Ctx#handle_info_guild_event_guild_members_provisioned.event)
    ),
    {ok, Ctx#handle_info_guild_event_guild_members_provisioned{
        members_with_roles = Members
    }}.



handle_info_guild_event_guild_members_provisioned_detect_members(Ctx) ->
    {Ctx, {AddRoleActiveMembers, RemoveRoleActiveMembers}} = lists:foldl(
        fun handle_info_guild_event_guild_members_provisioned_detect_members_fold/2,
        _Acc = {Ctx, {_AddRoleActiveMembers = [], _RemoveRoleActiveMembers = []}},
        Ctx#handle_info_guild_event_guild_members_provisioned.members_with_roles
    ),
    ?l_alert(#{
        text => "Plugin managing role-active for users",
        what => handle_info,
        details => #{
            guild_id => marvin_guild_context:guild_id(
                marvin_guild_pubsub:guild_context(
                    Ctx#handle_info_guild_event_guild_members_provisioned.event)),
            add_role_active_count => length(AddRoleActiveMembers),
            remove_role_active_count => length(RemoveRoleActiveMembers)
        }
    }),
    [
        marvin_rest2:enqueue_request(marvin_rest2_request:new(
            marvin_rest2_impl_guild_member_role_add,
            #{
                <<"guild_id">> => marvin_guild_context:guild_id(
                    marvin_guild_pubsub:guild_context(
                        Ctx#handle_info_guild_event_guild_members_provisioned.event)),
                <<"user_id">> => marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
                <<"role_id">> => Ctx#handle_info_guild_event_guild_members_provisioned.role_active
            }, #{}
        )) || Member <- AddRoleActiveMembers
    ],
    [
        marvin_rest2:enqueue_request(marvin_rest2_request:new(
            marvin_rest2_impl_guild_member_role_delete,
            #{
                <<"guild_id">> => marvin_guild_context:guild_id(
                    marvin_guild_pubsub:guild_context(
                        Ctx#handle_info_guild_event_guild_members_provisioned.event)),
                <<"user_id">> => marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
                <<"role_id">> => Ctx#handle_info_guild_event_guild_members_provisioned.role_active
            }, #{}
        )) || Member <- RemoveRoleActiveMembers
    ],
    {ok, Ctx}.



handle_info_guild_event_guild_members_provisioned_detect_members_fold(
    Member,
    {Ctx, {AddRoleActiveMembers, RemoveRoleActiveMembers}}
) ->
    case detect_role_active_change(
        Ctx#handle_info_guild_event_guild_members_provisioned.role_active,
        Member,
        marvin_guild_pubsub:guild_context(
            Ctx#handle_info_guild_event_guild_members_provisioned.event)
    ) of
        add ->
            {Ctx, {[Member | AddRoleActiveMembers], RemoveRoleActiveMembers}};
        remove ->
            {Ctx, {AddRoleActiveMembers, [Member | RemoveRoleActiveMembers]}};
        none ->
            {Ctx, {AddRoleActiveMembers, RemoveRoleActiveMembers}}
    end.



detect_role_active_change(RoleActive, Member, GuildCtx) ->
    case {
        %% Check if member has online status
        marvin_guild_helper_presence:r_get_member_status(
            marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
            GuildCtx
        ),
        %% Check if user already have the game-active role
        lists:member(RoleActive, marvin_pdu2_object_member:roles(Member))
    } of
        %% Member is online, but does not have the role; add the role
        {<<"online">>, false} -> add;
        %% Member is online and already have the role; do nothing
        {<<"online">>, true} -> none;
        %% Member is not online and does not have the role; do nothing
        {_, false} -> none;
        %% Member is not online and still have the role; remove the role
        {_, true} -> remove
    end.
