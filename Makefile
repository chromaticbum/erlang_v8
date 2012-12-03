all: compile

compile: clean rebar_compile
	
rebar_compile:
	./rebar compile

clean:
	./rebar clean

console: compile
	erl -pa ../erlang_v8 -pa ebin/

test: eunit

eunit: compile
	./rebar -C rebar.tests.config eunit skip_deps=true
