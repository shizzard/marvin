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
	_build/default/rel/marvin/bin/marvin console

run-release-prod:
	_build/prod/rel/marvin/bin/marvin console

run-logtail:
	tail -F _build/default/rel/marvin/log/debug.log
