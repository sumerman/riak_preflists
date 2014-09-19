all: build

REBAR = ./rebar

clean:
	${REBAR} clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam

depends: 
	${REBAR} get-deps

build: depends
	${REBAR} compile

standalone:
	${REBAR} -C rebar.config.standalone get-deps compile

