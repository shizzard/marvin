-module(marvin_guild_helper_members).

-include("marvin_guild_state.hrl").

-export([handle_call_do_provision_chain/1]).



%% Interface



-spec handle_call_do_provision_chain({Struct :: marvin_pdu2_dispatch_guild_members_chunk:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_guild_members_chunk:t(),
        S1 :: state()
    }).

handle_call_do_provision_chain({Struct, S0}) ->
    Members = marvin_pdu2_dispatch_guild_members_chunk:members(Struct),
    marvin_log:info("Guild '~s' members chunk: ~p total", [S0#state.guild_id, length(Members)]),
    S1 = lists:foldl(fun set_member/2, S0, Members),
    {ok, {Struct, S1}}.



%% Internals



-spec set_member(Member :: marvin_pdu2_object_member:t(), S0 :: state()) ->
    Ret :: state().

set_member(Member, #state{member_state = MemberStateEts} = S0) ->
    ets:insert(MemberStateEts, #member{
        member_id = marvin_pdu2_object_user:id(marvin_pdu2_object_member:user(Member)),
        member = Member
    }),
    S0.
