-module(marvin_plugin_lfg_handler_guild_provisioned).

-include("marvin_plugin_lfg.hrl").
-include_lib("marvin_log/include/marvin_log.hrl").

-export([handle/2]).



handle(Event, S0) ->
    #{
        <<"role_game_prefix">> := RoleGamePrefix
    } = marvin_plugin_config:data(S0#state.config),
    GuildCtx = marvin_guild_pubsub:guild_context(Event),
    Roles = marvin_guild_helper_role:r_get_all_roles(GuildCtx),
    GameRoles = [
        marvin_pdu2_object_role:id(Role) || Role <- Roles,
        marvin_plugin_lfg:is_game_role_name(RoleGamePrefix, marvin_pdu2_object_role:name(Role))
    ],
    ?l_debug(#{
        text => "Plugin is provisioned with roles",
        what => handle_info,
        details => #{
            guild_id => S0#state.guild_id,
            roles_total => length(GameRoles)
        }
    }),
    {noreply, S0#state{game_roles = GameRoles}}.
