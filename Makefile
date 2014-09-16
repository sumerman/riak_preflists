all: build

REBAR = ./rebar

clean:
	${REBAR} clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam

# Dialyzer
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler

APP_PLT = ./.app_plt

dialyze_main_plt:
	dialyzer --build_plt --apps $(APPS)

dialyze_app_plt:
	dialyzer --add_to_plt deps/*/ebin ebin --output_plt $(APP_PLT)

dialyze: build 
	dialyzer -Wunmatched_returns -Werror_handling -Wrace_conditions --plt $(APP_PLT) ebin

distclean: clean
	git clean -fxd
	rm -rf deps

depends: 
	${REBAR} get-deps

build: depends
	${REBAR} compile

tests: build
	${REBAR} eunit skip_deps=true
