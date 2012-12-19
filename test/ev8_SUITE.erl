-module(ev8_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  run_script/1,
  script_origin/1,
  fields/1,
  multi_fields/1,
  wrapped_fun/1,
  call/1,
  global/1
  ]).

all() ->
  [run_script,
   script_origin,
   fields,
   multi_fields,
   wrapped_fun,
   call,
   global].

init_per_suite(Config) ->
  erlang_v8:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  [{vm, Vm}, {context, Context} | Config].

end_per_suite(Config) ->
  erlang_v8:stop(),
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

  {error, {js_error, _Info, _StackTrace}} = ev8:run_script(C, <<"i.myFun()">>),

  ok.

script_origin(Config) ->
  C = ?config(context, Config),

  {error, {js_error, _Info, StackTrace}} = ev8:run_script(C, {"my_origin.erl", 120}, <<"iDontExist">>),
  true = string:str(binary_to_list(StackTrace), "my_origin.erl:120:1") > 0,

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

  {error, invalid_object} = (catch ev8:get(C, 2, <<"heyThere">>)),
  {error, invalid_object} = (catch ev8:set(C, <<"godzilla">>, <<"heyThere">>, <<"dude">>)),

  FieldObj = ev8:run_script(C, <<"new Object">>),
  ev8:set(C, Obj, FieldObj, <<"godzilla strikes">>),
  <<"godzilla strikes">> = ev8:get(C, Obj, FieldObj),

  ok.

multi_fields(Config) ->
  C = ?config(context, Config),

  Obj = ev8:run_script(C, <<"new Object">>),
  FieldObj = ev8:run_script(C, <<"new Object">>),

  ev8:set(C, Obj, [{FieldObj, <<"fieldObj">>},
                   {true, <<"true">>},
                   {false, <<"false">>},
                   does_nothing]),

  <<"fieldObj">> = ev8:get(C, Obj, FieldObj),
  <<"true">> = ev8:get(C, Obj, true),
  <<"false">> = ev8:get(C, Obj, false),
  undefined = ev8:get(C, Obj, does_nothing),

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
  ev8:set(C, Obj, <<"myFun">>, fun(A, B) -> A + B end),
  Fun2 = ev8:get(C, Obj, <<"myFun">>),
  Arr = ev8:call(C, Obj, Fun, [<<",">>]),

  <<"hello">> = ev8:get(C, Arr, 0),
  <<"world">> = ev8:get(C, Arr, 1),

  {error, not_fun} = ev8:call(C, Fun, Obj, [<<",">>]),
  {error, args_not_list} = ev8:call(C, Obj, Fun, <<",">>),

  1337 = ev8:call(C, Fun2, [1330, 7]),

  ok.

global(Config) ->
  C = ?config(context, Config),

  ev8:set(C, global, <<"globalProp">>, 1337),
  1337 = ev8:run_script(C, <<"globalProp">>),

  ok.
