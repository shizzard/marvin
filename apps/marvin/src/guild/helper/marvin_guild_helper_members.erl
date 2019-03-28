-module(marvin_guild_helper_members).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([
    w_do_provision/2,
    w_member_update/2,
    r_get_member_by_user_id/2,
    r_get_members_by_role/2
]).



%% Interface



-spec w_do_provision(Members :: [marvin_pdu2_object_member:t()], Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_do_provision(Members, Ctx) ->
    ?l_debug(#{
        text => "Guild members provisioned",
        what => members_provision, result => ok,
        details => #{
            guild_id => marvin_guild_context:guild_id(Ctx),
            total => length(Members)
        }
    }),
    ets:insert(marvin_guild_context:member_state(Ctx), [#member{
        member_id = marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
        member = Member
    } || Member <- Members]),
    ok.



w_member_update(Struct, Ctx) ->
    UserId = marvin_pdu2_object_user:id(marvin_pdu2_dispatch_guild_member_update:user(Struct)),
    case r_get_member_by_user_id(UserId, Ctx) of
        undefined ->
            ?l_error(#{
                text => "Guild member update failure",
                what => member_update, result => not_found,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    member_id => UserId
                }
            }),
            ok;
        Member ->
            ets:insert(marvin_guild_context:member_state(Ctx), #member{
                member_id = UserId,
                member = marvin_pdu2_object_member:update(Member, marvin_pdu2_dispatch_guild_member_update:export(Struct))
            }),
            ?l_debug(#{
                text => "Guild member update",
                what => member_update, result => ok,
                details => #{
                    guild_id => marvin_guild_context:guild_id(Ctx),
                    member_id => UserId
                }
            }),
            ok
    end.



r_get_members_by_role(RoleId, Ctx) ->
    [
        Member#member.member || Member
        <- ets:tab2list(marvin_guild_context:member_state(Ctx)),
        lists:member(RoleId, marvin_pdu2_object_member:roles(Member#member.member))
    ].



-spec r_get_member_by_user_id(UserId :: marvin_pdu2:snowflake(), Ctx :: marvin_guild_context:t()) ->
    Ret :: marvin_pdu2_object_member:t() | undefined.

r_get_member_by_user_id(UserId, Ctx) ->
    case ets:lookup(marvin_guild_context:member_state(Ctx), UserId) of
        [] ->
            undefined;
        [#member{member_id = UserId, member = Member}] ->
            Member
    end.
