ELVIS=./bin/elvis
REBAR=./bin/rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR) compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR) dialyzer

elvis:
	@echo "Running elvis rock..."
	@$(ELVIS) rock

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR) eunit

relx:
	@echo "Running rebar3 as prod release"
	@$(REBAR) as prod release

test: xref eunit dialyzer

travis: compile test

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR) xref

.PHONY: clean compile dialyzer elvis eunit test travis xref
