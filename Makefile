PROJECT = erlang_v8
DIALYZER = dialyzer

all: compile

v8: c_src/v8/.git/config c_src/v8/out/native/libv8_base.a

submodules: c_src/v8/.git/config

c_src/v8/.git/config:
	git submodule init
	git submodule update

c_src/v8/out/native/libv8_base.a:
	cd ./c_src/v8/; make dependencies; make native

compile: submodules v8 rebar-compile
	
rebar-compile:
	./rebar compile

clean:
	./rebar clean

console: compile
	erl -pa ../erlang_v8 -pa ebin/ -s erlang_v8

test: eunit ct

eunit: compile
	./rebar -C rebar.tests.config eunit skip_deps=true

ct: compile
	rm -rf logs
	./rebar -C rebar.tests.config ct skip_deps=true || open logs/index.html

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns
