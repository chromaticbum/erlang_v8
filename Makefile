all: compile

compile: clean rebar_compile
	
rebar_compile:
	./rebar compile

clean:
	./rebar clean

console: compile
	erl -pa ../erlang_v8 -pa ebin/ -s ev8

test: eunit ct

eunit: compile
	./rebar -C rebar.tests.config eunit skip_deps=true

ct: compile
	rm -rf logs
	./rebar -C rebar.tests.config ct skip_deps=true || open logs/index.html
