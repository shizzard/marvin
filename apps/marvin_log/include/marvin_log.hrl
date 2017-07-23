-define(config_path, []).
-define(config_path_logdir, [<<"logdir">>]).
-define(formatter, {formatter, lager_default_formatter}).
-define(formatter_config, {formatter_config, [
    severity, " ", date, "T", time, "Z", " [",
    {marvin_mod, "?"}, "/",
    {module, "?"}, ":",
    {line, "?"}, "@",
    {pid,"<?.?.?>"},
    "] ", message, "\n"
]}).
