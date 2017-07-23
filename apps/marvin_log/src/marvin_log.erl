-module(marvin_log).
-marvin_mod(marvin_log).

-compile({no_auto_import, [error/2]}).
-export([debug/2, info/2, notice/2, warn/2, error/2, crit/2, alert/2, emerg/2]).



%% Interface



-spec debug(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

debug(Format, Args) ->
    lager:log(debug, self(), Format, Args).



-spec info(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

info(Format, Args) ->
    lager:log(info, self(), Format, Args).



-spec notice(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

notice(Format, Args) ->
    lager:log(notice, self(), Format, Args).



-spec warn(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

warn(Format, Args) ->
    lager:log(warning, self(), Format, Args).



-spec error(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

error(Format, Args) ->
    lager:log(error, self(), Format, Args).



-spec crit(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

crit(Format, Args) ->
    lager:log(critical, self(), Format, Args).



-spec alert(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

alert(Format, Args) ->
    lager:log(alert, self(), Format, Args).



-spec emerg(Format :: string(), Args :: [term()]) ->
    marvin_helper_type:generic_return(ErrorRet :: lager_not_running).

emerg(Format, Args) ->
    lager:log(emergency, self(), Format, Args).
