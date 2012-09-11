ERL ?= erl
APP = tcpproxy

.PHONY: deps

all: deps
		@./rebar compile
		@dialyzer ./ebin --plt .dialyzer.plt -Wunmatched_returns -Werror_handling -Wunderspecs

app:
		@./rebar compile skip_deps=true
		@dialyzer ./ebin --plt .dialyzer.plt -Wunmatched_returns -Werror_handling -Wunderspecs

build_plt:
	@dialyzer --build_plt --output_plt .dialyzer.plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets xmerl webtool snmp public_key mnesia edoc syntax_tools compiler ./deps/*/ebin

deps:
		@./rebar get-deps

clean:
		@./rebar clean

distclean: clean
		@./rebar delete-deps

start: app
		exec erl -sname tcpproxy -pa ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s tcpproxy -s +K true +A 5 -env ERL_MAX_PORTS 4096 -env ERL_FULLSWEEP_AFTER 10

