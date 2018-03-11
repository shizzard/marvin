-module(prop_marvin_pdu2_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(PROPTEST(A), ?_assert(proper:quickcheck(A, [{to_file, user}]))).
-define(MAP(Typedef), ?LET(Proplist, Typedef, maps:from_list(lists:flatten(Proplist)))).
-define(OPTIONAL(FieldTypedef), oneof([[FieldTypedef], []])).


%% Tests


can_construct_test_() ->
    [?PROPTEST(can_construct(PDUMod)) || PDUMod <- pdu_mods() ++ object_mods()].

can_export_test_() ->
    [?PROPTEST(can_export(PDUMod)) || PDUMod <- pdu_mods() ++ object_mods()].

can_render_test_() ->
    [?PROPTEST(can_render(PDUMod)) || PDUMod <- pdu_mods()].

can_construct(PDUMod) ->
    ?FORALL(PDU, erlang:apply(?MODULE, PDUMod, []), can_construct(PDU, PDUMod)).

can_export(PDUMod) ->
    ?FORALL(PDU, erlang:apply(?MODULE, PDUMod, []), can_export(PDU, PDUMod)).

can_render(PDUMod) ->
    ?FORALL(PDU, erlang:apply(?MODULE, PDUMod, []), can_render(PDU, PDUMod)).


%% Internals


pdu_mods() ->
    [
        marvin_pdu2_heartbeat,
        marvin_pdu2_heartbeat_ack,
        marvin_pdu2_identify,
        marvin_pdu2_resume,
        marvin_pdu2_hello,
        marvin_pdu2_invalid_session,
        marvin_pdu2_dispatch_ready,
        marvin_pdu2_dispatch_resumed
    ].

object_mods() ->
    [
        marvin_pdu2_identify_properties,
        marvin_pdu2_object_user,
        marvin_pdu2_object_guild_unavailable,
        marvin_pdu2_object_channel_dm,
        marvin_pdu2_object_role,
        marvin_pdu2_object_member,
        marvin_pdu2_object_emoji
    ].


can_construct(PDU, PDUMod) ->
    try
        io:format("~p~n", [PDUMod:new(PDU)]),
        Struct = PDUMod:new(PDU),
        is_tuple(Struct)
    catch
        throw:badarg ->
            false
    end.


can_export(PDU, PDUMod) ->
    try
        io:format("~p~n", [PDUMod:export(PDUMod:new(PDU))]),
        NewPDU = PDUMod:export(PDUMod:new(PDU)),
        ensure_same_pdus(PDU, NewPDU),
        true
    catch
        throw:badarg ->
            false;
        throw:not_same_pdus ->
            false
    end.


can_render(PDU, PDUMod) ->
    try
        io:format("~p~n", [marvin_pdu2:render(PDUMod:new(PDU))]),
        {ok, JSON} = marvin_pdu2:render(PDUMod:new(PDU)),
        is_binary(JSON)
    catch
        throw:badarg ->
            false
    end.


ensure_same_pdus(OriginalPDU, ResultPDU) ->
    maps:fold(fun ensure_same_pdus_fold/3, ResultPDU, OriginalPDU).


ensure_same_pdus_fold(Key, Value, ResultPDU) ->
    case maps:get(list_to_binary(atom_to_list(Key)), ResultPDU) of
        Value ->
            ResultPDU;
        ReturnValue when is_map(ReturnValue) ->
            maps:fold(fun ensure_same_pdus_fold/3, ReturnValue, Value),
            ResultPDU;
        ReturnValue when is_list(ReturnValue) ->
            % should fix this eventually
            ResultPDU;
        _ ->
            throw(not_same_pdus)
    end.


%% Generators - PDUs


marvin_pdu2_heartbeat() ->
    ?MAP([{plain_value, marvin_pdu2_heartbeat:sequence()}]).

marvin_pdu2_identify_properties() ->
    ?MAP([
        {'$os', non_empty(proper_unicode:utf8(30))},
        {'$browser', non_empty(proper_unicode:utf8(30))},
        {'$device', non_empty(proper_unicode:utf8(30))},
        {'$referrer', non_empty(proper_unicode:utf8(30))},
        {'$referring_domain', non_empty(proper_unicode:utf8(30))}
    ]).

marvin_pdu2_identify() ->
    ?MAP([
        {token, non_empty(proper_unicode:utf8(30))},
        {compress, marvin_pdu2_identify:compress()},
        {large_threshold, marvin_pdu2_identify:large_threshold()},
        {properties, marvin_pdu2_identify_properties()},
        {shard, ?SUCHTHAT(
            [Shard, TotalShards],
            [marvin_pdu2:shard(), marvin_pdu2:total_shards()],
            Shard < TotalShards
        )}
    ]).

marvin_pdu2_resume() ->
    ?MAP([
        {token, non_empty(proper_unicode:utf8(30))},
        {session_id, non_empty(proper_unicode:utf8(30))},
        {seq, marvin_pdu2_resume:sequence()}
    ]).

marvin_pdu2_hello() ->
    ?MAP([
        {heartbeat_interval, marvin_pdu2_hello:heartbeat_interval()},
        {'_trace', list(non_empty(proper_unicode:utf8(20)))}
    ]).

marvin_pdu2_invalid_session() ->
    #{}.

marvin_pdu2_heartbeat_ack() ->
    #{}.


marvin_pdu2_dispatch_ready() ->
    ?MAP([
        {guilds, list(marvin_pdu2_object_guild_unavailable())},
        {private_channels, list(marvin_pdu2_object_channel_dm())},
        {session_id, non_empty(proper_unicode:utf8(30))},
        {user, marvin_pdu2_object_user()},
        {v, marvin_pdu2_dispatch_ready:protocol_version()},
        {'_trace', list(non_empty(proper_unicode:utf8(20)))}
    ]).

marvin_pdu2_dispatch_resumed() ->
    ?MAP([
        {'_trace', list(non_empty(proper_unicode:utf8(20)))}
    ]).


%% Generators - Objects


marvin_pdu2_object_user() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {username, non_empty(proper_unicode:utf8(15))},
        {discriminator, non_empty(proper_unicode:utf8(4))},
        {avatar, non_empty(proper_unicode:utf8(20))},
        ?OPTIONAL({bot, marvin_pdu2_object_user:bot()}),
        ?OPTIONAL({mfa_enabled, marvin_pdu2_object_user:mfa_enabled()}),
        ?OPTIONAL({verified, marvin_pdu2_object_user:verified()}),
        ?OPTIONAL({email, non_empty(proper_unicode:utf8(15))})
    ]).

marvin_pdu2_object_guild_unavailable() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8())},
        {unavailable, marvin_pdu2_object_guild_unavailable:unavailable()}
    ]).

marvin_pdu2_object_channel_dm() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {type, 1},
        {last_message_id, non_empty(proper_unicode:utf8(20))},
        {recipients, list(marvin_pdu2_object_user())}
    ]).

marvin_pdu2_object_role() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {name, non_empty(proper_unicode:utf8(15))},
        {permissions, marvin_pdu2_object_role:permissions()},
        {position, marvin_pdu2_object_role:position()},
        {color, marvin_pdu2_object_role:color()},
        {mentionable, marvin_pdu2_object_role:mentionable()},
        {managed, marvin_pdu2_object_role:managed()},
        {hoist, marvin_pdu2_object_role:hoist()}
    ]).

marvin_pdu2_object_member() ->
    ?MAP([
        {user, marvin_pdu2_object_user()},
        {roles, list(non_empty(proper_unicode:utf8(20)))},
        {nick, non_empty(proper_unicode:utf8(15))},
        {mute, marvin_pdu2_object_member:mute()},
        {joined_at, non_empty(proper_unicode:utf8(16))},
        {deaf, marvin_pdu2_object_member:deaf()}
    ]).

marvin_pdu2_object_emoji() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {name, non_empty(proper_unicode:utf8(15))},
        {roles, list(non_empty(proper_unicode:utf8(20)))},
        {require_colons, marvin_pdu2_object_emoji:require_colons()},
        {managed, marvin_pdu2_object_emoji:managed()},
        {animated, marvin_pdu2_object_emoji:animated()}
    ]).
