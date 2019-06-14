REBAR = ./rebar3

.PHONY: all get-deps compile release test shell clean dialyze run-release run-logtail

all: get-deps compile release

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

release:
	$(REBAR) release

release-prod:
	$(REBAR) as prod release

shell: compile
	$(REBAR) shell

clean:
	$(REBAR) clean

dialyze:
	$(REBAR) dialyzer

test:
	$(REBAR) eunit

clean-release:
	rm -rf _build/default/rel

clean-release-prod:
	rm -rf _build/prod/rel

run-release:
	RELX_REPLACE_OS_VARS=true \
	MARVIN_VMARGS_SNAME=marvin \
	MARVIN_VMARGS_COOKIE=marvin-cookie \
	MARVIN_VMARGS_LIMIT_ETS=1024 \
	MARVIN_VMARGS_LIMIT_PROCESSES=64535 \
	MARVIN_VMARGS_LIMIT_PORTS=1024 \
	MARVIN_VMARGS_LIMIT_ATOMS=1048576 \
	MARVIN_VMARGS_ASYNC_THREADS=8 \
	MARVIN_VMARGS_KERNEL_POLL=true \
	MARVIN_VMARGS_SMP=auto \
	MARVIN_APP_API_HOST=discordapp.com \
	MARVIN_APP_API_PORT=443 \
	MARVIN_APP_API_ROOT_URL=/api \
	MARVIN_APP_API_GATEWAY_URL=/gateway/bot \
	MARVIN_APP_GATEWAY_PORT=443 \
	MARVIN_APP_GATEWAY_PROTOVER=6 \
	MARVIN_APP_GATEWAY_COMPRESS=false \
	MARVIN_APP_GATEWAY_LARGE_THRESHOLD=50 \
	MARVIN_APP_DIALOGFLOW_HOST=api.dialogflow.com \
	MARVIN_APP_DIALOGFLOW_PORT=443 \
	MARVIN_APP_DIALOGFLOW_ROOT_URL=/v1 \
	MARVIN_APP_SYSINFO_LIBRARY_NAME=Marvin \
	MARVIN_APP_SYSINFO_LIBRARY_WEB=http://shizzard.github.io/marvin \
	MARVIN_APP_LOGGER_LOG_ROOT=log/ \
	MARVIN_APP_LOGGER_LOG_LEVEL=debug \
	MARVIN_APP_GUILD_CONFIG_ROOT=`pwd`/data/guild/ \
	MARVIN_APP_GUILD_CONFIG_FILENAME_TEMPLATE=guild_{{guild_id}}_config.json \
	MARVIN_APP_PLUGIN_CONFIG_ROOT=`pwd`/data/plugin/ \
	MARVIN_APP_PLUGIN_CONFIG_FILENAME_TEMPLATE=plugin_{{plugin_id}}_{{guild_id}}_config.json \
	MARVIN_STORAGE_DB_POOL_SIZE=10 \
	MARVIN_STORAGE_DB_POOL_OVERFLOW=10 \
	MARVIN_STORAGE_DB_NAME=marvin \
	MARVIN_STORAGE_DB_HOST=localhost \
	MARVIN_STORAGE_DB_PORT=27017 \
	_build/default/rel/marvin/bin/marvin console

run-logtail:
	multitail -CS marvin _build/default/rel/marvin/log/lgr_info.log.1 _build/default/rel/marvin/log/lgr_error.log.1
