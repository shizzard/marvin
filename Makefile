REBAR = ./rebar3

.PHONY: all get-deps compile release shell clean dialyze run-release run-tests

all: get-deps compile release

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

release: compile
	$(REBAR) release

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
