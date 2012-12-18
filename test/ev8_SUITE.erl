-module(ev8_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  run_script/1,
  fields/1,
  wrapped_fun/1,
  call/1
  ]).

all() ->
  [run_script,
   fields,
   wrapped_fun,
   call].

init_per_suite(Config) ->
  ev8:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  [{vm, Vm}, {context, Context} | Config].

end_per_suite(Config) ->
  ev8:stop(),
  Config.

run_script(Config) ->
  C = ?config(context, Config),

  undefined = ev8:run_script(C, <<"undefined">>),
  null = ev8:run_script(C, <<"null">>),
  22 = ev8:run_script(C, <<"22">>),
  -22 = ev8:run_script(C, <<"-22">>),
  22.2 = ev8:run_script(C, <<"22.2">>),
  true = ev8:run_script(C, <<"true">>),
  false = ev8:run_script(C, <<"false">>),
  <<"hello">> = ev8:run_script(C, <<"'hello'">>),
  <<>> = ev8:run_script(C, <<"new Object()">>),

  {error, {js_compiler_error, _Info, _StackTrace}} = ev8:run_script(C, <<"i.myFun()">>),

  ok.

fields(Config) ->
  C = ?config(context, Config),

  Obj = ev8:run_script(C, <<"new Object()">>),
  ev8:set(C, Obj, <<"erlUndefined">>, undefined),
  ev8:set(C, Obj, <<"erlNull">>, null),
  ev8:set(C, Obj, <<"erlInt">>, 22),
  ev8:set(C, Obj, <<"erlNegInt">>, -22),
  ev8:set(C, Obj, <<"erlDouble">>, 22.2),
  ev8:set(C, Obj, <<"erlTrue">>, true),
  ev8:set(C, Obj, <<"erlFalse">>, false),
  ev8:set(C, Obj, <<"erlBinary">>, <<"godzilla strikes">>),
  ev8:set(C, Obj, <<"erlList">>, [<<"hello">>, <<"there">>, [true, false, null, undefined]]),

  undefined = ev8:get(C, Obj, <<"erlUndefined">>),
  null = ev8:get(C, Obj, <<"erlNull">>),
  22 = ev8:get(C, Obj, <<"erlInt">>),
  -22 = ev8:get(C, Obj, <<"erlNegInt">>),
  22.2 = ev8:get(C, Obj, <<"erlDouble">>),
  true = ev8:get(C, Obj, <<"erlTrue">>),
  false = ev8:get(C, Obj, <<"erlFalse">>),
  <<"godzilla strikes">> = ev8:get(C, Obj, <<"erlBinary">>),
  [<<"hello">>, <<"there">>, [true, false, null, undefined]] = ev8:get(C, Obj, <<"erlList">>),

  {'EXIT',{badarg,_}} = (catch ev8:get(C, 2, <<"heyThere">>)),
  {'EXIT',{badarg,_}} = (catch ev8:set(C, <<"godzilla">>, <<"heyThere">>, <<"dude">>)),

  FieldObj = ev8:run_script(C, <<"new Object">>),
  ev8:set(C, Obj, FieldObj, <<"godzilla strikes">>),
  <<"godzilla strikes">> = ev8:get(C, Obj, FieldObj),

  ok.

wrapped_fun(Config) ->
  C = ?config(context, Config),

  Obj = ev8:run_script(C, <<"var a = new Object(); a">>),
  ev8:set(C, Obj, <<"erlFun">>, fun(A, B) -> A + B end),

  6 = ev8:run_script(C, <<"a.erlFun(2, 4)">>),

  ok.

call(Config) ->
  C = ?config(context, Config),

  Obj = ev8:run_script(C, <<"new String('hello,world')">>),
  Fun = ev8:get(C, Obj, <<"split">>),
  Arr = ev8:call(C, Obj, Fun, [<<",">>]),

  <<"hello">> = ev8:get(C, Arr, 0),
  <<"world">> = ev8:get(C, Arr, 1),

  {error, not_fun} = ev8:call(C, Fun, Obj, [<<",">>]),
  {error, args_not_list} = ev8:call(C, Obj, Fun, <<",">>),

  ok.
