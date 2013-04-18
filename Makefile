PROJECT = giallo
DIALYZER = dialyzer
REBAR = ./rebar

.PHONY: all deps compile clean test ct build-plt dialyze

all: deps compile

deps:
	$(REBAR) get-deps

doc: deps
	$(REBAR) doc skip_deps=true

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

test: ct dialyze doc

test-build:
	$(REBAR) -C rebar.test.config compile

ct: clean deps test-build
	$(REBAR) -C rebar.test.config ct skip_deps=true

build-plt:
	$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib sasl inets crypto public_key ssl \
		./deps/cowboy/ebin ./deps/erlydtl/ebin ./deps/jsx/ebin \
		./deps/mimetypes/ebin ./deps/ranch/ebin

dialyze: clean deps test-build
	$(DIALYZER) --plt .$(PROJECT).plt ebin \
		-Werror_handling -Wrace_conditions -Wunmatched_returns
