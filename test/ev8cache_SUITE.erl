-module(ev8cache_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  eval_file/1
  ]).

all() ->
  [eval_file].

init_per_suite(Config) ->
  erlang_v8:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  [{vm, Vm}, {context, Context} | Config].

end_per_suite(Config) ->
  erlang_v8:stop(),
  Config.

eval_file(Config) ->
  C = ?config(context, Config),

  ev8:set(C, global, <<"cache_val">>, 0),
  ev8cache:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/cache.js")),
  1 = evo8:get(C, global, <<"cache_val">>),
  ev8cache:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/cache.js")),
  ev8cache:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/cache.js")),
  ev8cache:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/cache.js")),
  1 = evo8:get(C, global, <<"cache_val">>),

  ok.
