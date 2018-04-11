-module(marvin_guild_helper_message).

-include("marvin_guild_state.hrl").

-export([
    handle_call_message_create_chain/1
]).



%% Interface



-spec handle_call_message_create_chain({Struct :: marvin_pdu2_dispatch_message_create:t(), S0 :: state()}) ->
    marvin_helper_type:ok_return(OkRet :: {
        Struct :: marvin_pdu2_dispatch_message_create:t(),
        S1 :: state()
    }).

handle_call_message_create_chain({Struct, S0}) ->
    {ok, {Struct, S0}}.
