REBAR = ./rebar3

.PHONY: all get-deps compile compile-debug release shell clean dialyze run-release run-tests run-logtail

all: get-deps compile release

get-deps:
	$(REBAR) get-deps

compile:
	ERL_COMPILER_OPTIONS='[{d, cloak_dump, "temp"}]' $(REBAR) compile

release:
	$(REBAR) release

shell: compile
	$(REBAR) shell

clean:
	$(REBAR) clean

dialyze:
	$(REBAR) dialyzer

run-tests:
	$(REBAR) eunit

run-release:
	_build/default/rel/marvin/bin/marvin console

run-logtail:
	tail -F _build/default/rel/marvin/log/debug.log
