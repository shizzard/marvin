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
	MARVIN_APP_SYSINFO_LIBRARY_NAME=Marvin \
	MARVIN_APP_SYSINFO_LIBRARY_WEB=http://shizzard.github.io/marvin \
	MARVIN_APP_LAGER_LOG_ROOT=log/ \
	_build/default/rel/marvin/bin/marvin console

run-logtail:
	tail -F _build/default/rel/marvin/log/debug.log
