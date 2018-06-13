-module(marvin_guild_helper_repl).
-include("marvin_guild_state.hrl").

-export([
    dump_channel_categories/1, dump_channel_text/1, dump_channel_voice/1,
    dump_members/1, dump_roles/1
]).



%% Interface



dump_channel_categories(GuildId) ->
    {ok, Ctx} = marvin_guild:get_context(GuildId),
    lists:map(fun(#channel{channel_id = Id, channel = Item}) ->
        io:format("'~ts' '~ts'~n", [Id, marvin_pdu2_object_channel:name(Item)])
    end, ets:tab2list(marvin_guild_context:channel_category_state(Ctx))),
    ok.



dump_channel_text(GuildId) ->
    {ok, Ctx} = marvin_guild:get_context(GuildId),
    lists:map(fun(#channel{channel_id = Id, channel = Item}) ->
        io:format("'~ts' '~ts'~n", [Id, marvin_pdu2_object_channel:name(Item)])
    end, ets:tab2list(marvin_guild_context:channel_text_state(Ctx))),
    ok.



dump_channel_voice(GuildId) ->
    {ok, Ctx} = marvin_guild:get_context(GuildId),
    lists:map(fun(#channel{channel_id = Id, channel = Item}) ->
        io:format("'~ts' '~ts'~n", [Id, marvin_pdu2_object_channel:name(Item)])
    end, ets:tab2list(marvin_guild_context:channel_voice_state(Ctx))),
    ok.



dump_members(GuildId) ->
    {ok, Ctx} = marvin_guild:get_context(GuildId),
    lists:map(fun(#member{member_id = Id, member = Item}) ->
        io:format("'~ts' '~ts#~ts'~n", [
            Id,
            marvin_pdu2_object_user:username(marvin_pdu2_object_member:user(Item)),
            marvin_pdu2_object_user:discriminator(marvin_pdu2_object_member:user(Item))
        ])
    end, ets:tab2list(marvin_guild_context:member_state(Ctx))),
    ok.



dump_roles(GuildId) ->
    {ok, Ctx} = marvin_guild:get_context(GuildId),
    lists:map(fun(#role{role_id = Id, role = Item}) ->
        io:format("'~ts' '~ts'~n", [Id, marvin_pdu2_object_role:name(Item)])
    end, ets:tab2list(marvin_guild_context:role_state(Ctx))),
    ok.
