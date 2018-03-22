-module(prop_marvin_pdu2_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(PROPTEST(A), {
    timeout, 100,
    ?_assert(proper:quickcheck(A, [{to_file, user}]))
}).
-define(MAP(Typedef), ?LET(Proplist, Typedef, maps:from_list(lists:flatten(Proplist)))).
-define(OPTIONAL(FieldTypedef), oneof([[FieldTypedef], []])).
-define(NULLABLE(FieldTypedef), oneof([FieldTypedef, null])).


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
        marvin_pdu2_dispatch_resumed,
        marvin_pdu2_dispatch_guild_create,
        marvin_pdu2_dispatch_presence_update
    ].

object_mods() ->
    [
        marvin_pdu2_identify_properties,
        marvin_pdu2_object_user,
        marvin_pdu2_object_guild_unavailable,
        marvin_pdu2_object_role,
        marvin_pdu2_object_member,
        marvin_pdu2_object_emoji,
        marvin_pdu2_object_permission_overwrite,
        marvin_pdu2_object_channel,
        marvin_pdu2_object_game,
        marvin_pdu2_object_presence,
        marvin_pdu2_object_voice_state
    ].


can_construct(PDU, PDUMod) ->
    try
        Struct = PDUMod:new(PDU),
        is_tuple(Struct)
    catch
        throw:badarg ->
            ?debugMsg(PDUMod),
            false
    end.


can_export(PDU, PDUMod) ->
    try
        NewPDU = PDUMod:export(PDUMod:new(PDU)),
        ensure_same_pdus(PDU, NewPDU),
        true
    catch
        throw:badarg ->
            ?debugMsg(PDUMod),
            false;
        throw:not_same_pdus ->
            ?debugMsg(PDUMod),
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

marvin_pdu2_dispatch_guild_create() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {name, non_empty(proper_unicode:utf8(15))},
        {icon, non_empty(proper_unicode:utf8(15))},
        {splash, non_empty(proper_unicode:utf8(15))},
        {owner, marvin_pdu2_dispatch_guild_create:owner()},
        {owner_id, non_empty(proper_unicode:utf8(20))},
        {permissions, marvin_pdu2_dispatch_guild_create:permissions()},
        {region, non_empty(proper_unicode:utf8(15))},
        {afk_channel_id, non_empty(proper_unicode:utf8(20))},
        {afk_timeout, marvin_pdu2_dispatch_guild_create:afk_timeout()},
        {embed_enabled, marvin_pdu2_dispatch_guild_create:embed_enabled()},
        {embed_channel_id, non_empty(proper_unicode:utf8(20))},
        {verification_level, marvin_pdu2_dispatch_guild_create:verification_level()},
        {default_message_notifications, marvin_pdu2_dispatch_guild_create:default_message_notifications()},
        {explicit_content_filter, marvin_pdu2_dispatch_guild_create:explicit_content_filter()},
        {roles, list(marvin_pdu2_object_role())},
        {emojis, list(marvin_pdu2_object_emoji())},
        {features, list(non_empty(proper_unicode:utf8(15)))},
        {mfa_level, marvin_pdu2_dispatch_guild_create:mfa_level()},
        {application_id, non_empty(proper_unicode:utf8(20))},
        {widget_enabled, marvin_pdu2_dispatch_guild_create:widget_enabled()},
        {widget_channel_id, non_empty(proper_unicode:utf8(20))},
        {system_channel_id, non_empty(proper_unicode:utf8(20))},
        {joined_at, non_empty(proper_unicode:utf8(15))},
        {large, marvin_pdu2_dispatch_guild_create:large()},
        {unavailable, marvin_pdu2_dispatch_guild_create:unavailable()},
        {member_count, marvin_pdu2_dispatch_guild_create:member_count()},
        {voice_states, list(marvin_pdu2_object_voice_state())},
        {members, list(marvin_pdu2_object_member())},
        {channels, list(marvin_pdu2_object_channel())},
        {presences, list(marvin_pdu2_object_presence())}
    ]).

marvin_pdu2_dispatch_presence_update() ->
    ?MAP([
        {user, ?MAP([{<<"id">>, non_empty(proper_unicode:utf8(20))}])},
        {nick, ?NULLABLE(non_empty(proper_unicode:utf8(15)))},
        {roles, list(non_empty(proper_unicode:utf8(20)))},
        {game, ?NULLABLE(marvin_pdu2_object_game())},
        {guild_id, non_empty(proper_unicode:utf8(20))},
        {status, oneof([<<"online">>, <<"offline">>, <<"idle">>, <<"dnd">>])}
    ]).


%% Generators - Objects


marvin_pdu2_object_user() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {username, non_empty(proper_unicode:utf8(15))},
        {discriminator, non_empty(proper_unicode:utf8(4))},
        {avatar, ?NULLABLE(non_empty(proper_unicode:utf8(20)))},
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
        ?OPTIONAL({nick, ?NULLABLE(non_empty(proper_unicode:utf8(15)))}),
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

marvin_pdu2_object_permission_overwrite() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {type, oneof([<<"member">>, <<"role">>])},
        {deny, marvin_pdu2_object_permission_overwrite:deny()},
        {allow, marvin_pdu2_object_permission_overwrite:allow()}
    ]).

marvin_pdu2_object_channel() ->
    oneof([
        marvin_pdu2_object_channel_text(),
        marvin_pdu2_object_channel_dm(),
        marvin_pdu2_object_channel_voice()
        % marvin_pdu2_object_channel_category()
    ]).

marvin_pdu2_object_channel_text() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        ?OPTIONAL({guild_id, non_empty(proper_unicode:utf8(20))}),
        {type, 0},
        {parent_id, non_empty(proper_unicode:utf8(20))},
        {name, non_empty(proper_unicode:utf8(20))},
        {topic, non_empty(proper_unicode:utf8(20))},
        {last_message_id, non_empty(proper_unicode:utf8(20))},
        {position, marvin_pdu2_object_channel:position()},
        {permission_overwrites, list(marvin_pdu2_object_permission_overwrite())}
    ]).

marvin_pdu2_object_channel_dm() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        {type, 1},
        {last_message_id, non_empty(proper_unicode:utf8(20))},
        {recipients, list(marvin_pdu2_object_user())}
    ]).

marvin_pdu2_object_channel_voice() ->
    ?MAP([
        {id, non_empty(proper_unicode:utf8(20))},
        ?OPTIONAL({guild_id, non_empty(proper_unicode:utf8(20))}),
        {type, 2},
        {parent_id, non_empty(proper_unicode:utf8(20))},
        {name, non_empty(proper_unicode:utf8(20))},
        {position, marvin_pdu2_object_channel:position()},
        {user_limit, marvin_pdu2_object_channel:user_limit()},
        {bitrate, marvin_pdu2_object_channel:bitrate()},
        {permission_overwrites, list(marvin_pdu2_object_permission_overwrite())}
    ]).

marvin_pdu2_object_game() ->
    oneof([
        marvin_pdu2_object_game_playing(),
        marvin_pdu2_object_game_streaming(),
        marvin_pdu2_object_game_listening()
    ]).

marvin_pdu2_object_game_playing() ->
    ?MAP([
        {type, 0},
        {timestamps, ?MAP([
            {<<"start">>, marvin_pdu2_object_game:timestamps()}
        ])},
        {name, non_empty(proper_unicode:utf8(20))}
    ]).

marvin_pdu2_object_game_streaming() ->
    ?MAP([
        {type, 0},
        {url, non_empty(proper_unicode:utf8(20))},
        {name, non_empty(proper_unicode:utf8(30))},
        {details, non_empty(proper_unicode:utf8(20))}
    ]).

marvin_pdu2_object_game_listening() ->
    ?MAP([
        {type, 0},
        {name, non_empty(proper_unicode:utf8(30))}
    ]).

marvin_pdu2_object_presence() ->
    ?MAP([
        {user, ?MAP([
            {<<"id">>, non_empty(proper_unicode:utf8(20))}
        ])},
        {game, ?NULLABLE(marvin_pdu2_object_game())},
        {status, oneof([<<"online">>, <<"offline">>, <<"idle">>, <<"dnd">>])}
    ]).

marvin_pdu2_object_voice_state() ->
    ?MAP([
        {user_id, non_empty(proper_unicode:utf8(20))},
        ?OPTIONAL({guild_id, non_empty(proper_unicode:utf8(20))}),
        {channel_id, ?NULLABLE(non_empty(proper_unicode:utf8(20)))},
        {session_id, non_empty(proper_unicode:utf8(20))},
        {suppress, marvin_pdu2_object_voice_state:suppress()},
        {self_video, marvin_pdu2_object_voice_state:self_video()},
        {self_mute, marvin_pdu2_object_voice_state:self_mute()},
        {self_deaf, marvin_pdu2_object_voice_state:self_deaf()},
        {mute, marvin_pdu2_object_voice_state:mute()},
        {deaf, marvin_pdu2_object_voice_state:deaf()}
    ]).
