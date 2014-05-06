.PHONY: deps

REBAR=./rebar

all: deps compile-all modules

modules:

compile:
	@$(REBAR) compile

compile-all: modules
	@$(REBAR) compile

app:
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps

clean-modules:

clean:
	@$(REBAR) clean

clean-all: clean-modules
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

test: app
	@$(REBAR) eunit skip_deps=true

console: app
	exec erl +pc unicode -pa $(PWD)/ebin \
	  -pa $(PWD)/deps/*/ebin \
		-config $(PWD)/priv/app.config \
		-s bcp47

