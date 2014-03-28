REBAR=`which rebar`
DIALYZER=`which dialyzer`
RELX=`which relx`

all: deps compile

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

app.plt:
	@$(DIALYZER) --build_plt --output_plt app.plt --apps erts kernel stdlib crypto

dialyze: app.plt compile
	@$(DIALYZER) -q --plt app.plt apps/*/ebin -Wunmatched_returns \
		-Werror_handling -Wrace_conditions -Wno_undefined_callbacks

test: compile
	@$(REBAR) eunit skip_deps=true verbose=0

validate: dialyze test

release: clean validate
	@$(RELX) release tar

relup: clean validate
	@$(RELX) release relup tar

repl:
	_rel/bin/locker

clean:
	@$(RM) -rf deps/
	@$(REBAR) clean

.PHONY: all test clean validate dialyze deps ct
