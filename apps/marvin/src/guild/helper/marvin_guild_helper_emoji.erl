-module(marvin_guild_helper_emoji).

-include("marvin_guild_state.hrl").

-export([handle_call_do_provision_chain/1]).


%% Interface


-spec handle_call_do_provision_chain({Struct :: marvin_pdu2_dispatch_guild_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_guild_create:t(),
        S1 :: state()
    }).

handle_call_do_provision_chain({Struct, S0}) ->
    S1 = lists:foldl(
        fun set_emoji/2, S0,
        marvin_pdu2_dispatch_guild_create:emojis(Struct)
    ),
    {ok, {Struct, S1}}.



%% Internals



-spec set_emoji(EmojiStruct :: marvin_pdu2_object_emoji:t(), S0 :: state()) ->
    Ret :: state().

set_emoji(EmojiStruct, #state{emoji_state = EmojiStateEts} = S0) ->
    ets:insert(EmojiStateEts, #emoji{
        emoji_id = marvin_pdu2_object_emoji:id(EmojiStruct),
        emoji = EmojiStruct
    }),
    S0.
