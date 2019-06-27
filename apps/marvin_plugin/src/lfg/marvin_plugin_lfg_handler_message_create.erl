-module(marvin_plugin_lfg_handler_message_create).

-include("marvin_plugin_lfg.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([handle_message_create/2]).



handle_message_create(Event, S0) ->
    #{
        <<"role_game_prefix">> := RoleGamePrefix,
        <<"channel_id">> := ChannelId
    } = marvin_plugin_config:data(S0#state.config),
    %% FIXME: change this shit
    _ = (marvin_plugin_lfg:get_pre_command_hook_fun(RoleGamePrefix, ChannelId))(Event),
    {noreply, S0}.
