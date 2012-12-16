-module(ev8_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  execute_script/1,
  execute_field/1,
  set_field/1
  ]).

all() ->
  [execute_script,
   execute_field,
   set_field].

init_per_suite(Config) ->
  ev8:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  [{vm, Vm}, {context, Context} | Config].

end_per_suite(Config) ->
  ev8:stop(),
  Config.

execute_script(Config) ->
  C = ?config(context, Config),

  undefined = ev8:execute_script(C, <<"undefined">>),
  null = ev8:execute_script(C, <<"null">>),
  22 = ev8:execute_script(C, <<"22">>),
  -22 = ev8:execute_script(C, <<"-22">>),
  22.2 = ev8:execute_script(C, <<"22.2">>),
  true = ev8:execute_script(C, <<"true">>),
  false = ev8:execute_script(C, <<"false">>),
  <<"hello">> = ev8:execute_script(C, <<"'hello'">>),
  <<>> = ev8:execute_script(C, <<"new Object()">>),

  ok.

execute_field(Config) ->
  _C = ?config(context, Config),

  ok.

set_field(_Config) ->
  ok.
