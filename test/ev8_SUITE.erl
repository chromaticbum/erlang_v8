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
  fields/1
  ]).

all() ->
  [execute_script,
   execute_field,
   fields].

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
  C = ?config(context, Config),

  Obj = ev8:execute_script(C, <<"new Object()">>),
  ev8:set_field(C, Obj, <<"erlFun">>, fun()-> <<"hello godzilla">> end),
  <<"hello godzilla">> = ev8:execute_field(C, Obj, <<"erlFun">>, [ok]),
  % TODO: allow passing of arguments

  ok.

fields(Config) ->
  C = ?config(context, Config),

  Obj = ev8:execute_script(C, <<"new Object()">>),
  ev8:set_field(C, Obj, <<"erlUndefined">>, undefined),
  ev8:set_field(C, Obj, <<"erlNull">>, null),
  ev8:set_field(C, Obj, <<"erlInt">>, 22),
  ev8:set_field(C, Obj, <<"erlNegInt">>, -22),
  ev8:set_field(C, Obj, <<"erlDouble">>, 22.2),
  ev8:set_field(C, Obj, <<"erlTrue">>, true),
  ev8:set_field(C, Obj, <<"erlFalse">>, false),
  ev8:set_field(C, Obj, <<"erlBinary">>, <<"godzilla strikes">>),
  ev8:set_field(C, Obj, <<"erlList">>, [<<"hello">>, <<"there">>, [true, false, null, undefined]]),

  undefined = ev8:get_field(C, Obj, <<"erlUndefined">>),
  null = ev8:get_field(C, Obj, <<"erlNull">>),
  22 = ev8:get_field(C, Obj, <<"erlInt">>),
  -22 = ev8:get_field(C, Obj, <<"erlNegInt">>),
  22.2 = ev8:get_field(C, Obj, <<"erlDouble">>),
  true = ev8:get_field(C, Obj, <<"erlTrue">>),
  false = ev8:get_field(C, Obj, <<"erlFalse">>),
  <<"godzilla strikes">> = ev8:get_field(C, Obj, <<"erlBinary">>),
  <<>> = ev8:get_field(C, Obj, <<"erlList">>),

  ok.