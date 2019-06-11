-include_lib("kernel/include/logger.hrl").

-define(l_debug(Map), ?LOG_DEBUG(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_info(Map), ?LOG_INFO(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_notice(Map), ?LOG_NOTICE(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_warning(Map), ?LOG_WARNING(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_error(Map), ?LOG_ERROR(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_critical(Map), ?LOG_CRITICAL(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_alert(Map), ?LOG_ALERT(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
-define(l_emergency(Map), ?LOG_EMERGENCY(marvin_log:log_map(Map), marvin_log:log_meta(Map))).
