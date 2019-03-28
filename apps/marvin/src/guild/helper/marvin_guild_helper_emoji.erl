-module(marvin_guild_helper_emoji).
-include("marvin_guild_state.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([w_do_provision/2]).



%% Interface



-spec w_do_provision(Emojis :: [marvin_pdu2_object_emoji:t()], Ctx :: marvin_guild_context:t()) ->
    marvin_helper_type:ok_return().

w_do_provision(Emojis, Ctx) ->
    ?l_debug(#{
        text => "Guild emojis provisioned",
        what => emoji_provision, result => ok,
        details => #{
            guild_id => marvin_guild_context:guild_id(Ctx),
            total => length(Emojis)
        }
    }),
    ets:insert(marvin_guild_context:emoji_state(Ctx), [#emoji{
        emoji_id = marvin_pdu2_object_emoji:id(Emoji),
        emoji = Emoji
    } || Emoji <- Emojis]),
    ok.
