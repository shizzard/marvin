-module(marvin_guild_helper_members).

-include("marvin_guild_state.hrl").

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
    marvin_log:info("Guild '~s' members chunk: ~p total", [marvin_guild_context:guild_id(Ctx), length(Members)]),
    ets:insert(marvin_guild_context:member_state(Ctx), [#member{
        member_id = marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
        member = Member
    } || Member <- Members]),
    ok.



w_member_update(Struct, Ctx) ->
    UserId = marvin_pdu2_object_user:id(marvin_pdu2_dispatch_guild_member_update:user(Struct)),
    case r_get_member_by_user_id(UserId, Ctx) of
        undefined ->
            marvin_log:error(
                "Guild '~s' got unknown member update: ~p",
                [marvin_guild_context:guild_id(Ctx), Struct]
            ),
            ok;
        Member ->
            ets:insert(marvin_guild_context:member_state(Ctx), #member{
                member_id = UserId,
                member = marvin_pdu2_object_member:update(Member, marvin_pdu2_dispatch_guild_member_update:export(Struct))
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
