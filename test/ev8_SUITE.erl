-module(ev8_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  eval/1,
  script_origin/1,
  fields/1,
  multi_fields/1,
  set_no_wrap/1,
  wrapped_fun/1,
  call/1,
  global/1,
  multi_context_call/1,
  arrays/1
  ]).

all() ->
  [eval,
   script_origin,
   fields,
   set_no_wrap,
   multi_fields,
   wrapped_fun,
   call,
   global,
   multi_context_call,
   arrays].

init_per_suite(Config) ->
  erlang_v8:start(),
  Vm = ev8:new_vm(),
  Context = ev8:new_context(Vm),
  [{vm, Vm}, {context, Context} | Config].

end_per_suite(Config) ->
  erlang_v8:stop(),
  Config.

eval(Config) ->
  C = ?config(context, Config),

  undefined = ev8:eval(C, <<"undefined">>),
  null = ev8:eval(C, <<"null">>),
  22 = ev8:eval(C, <<"22">>),
  -22 = ev8:eval(C, <<"-22">>),
  22.2 = ev8:eval(C, <<"22.2">>),
  true = ev8:eval(C, <<"true">>),
  false = ev8:eval(C, <<"false">>),
  <<"hello">> = ev8:eval(C, <<"'hello'">>),
  <<>> = ev8:eval(C, <<"new Object()">>),

  {error, {js_error, _Info, _StackTrace}} = ev8:eval(C, <<"i.myFun()">>),

  ok.

script_origin(Config) ->
  C = ?config(context, Config),

  {error, {js_error, _Info, StackTrace}} = ev8:eval(C, {"my_origin.erl", 120}, <<"iDontExist">>),
  true = string:str(binary_to_list(StackTrace), "my_origin.erl:120:1") > 0,

  ok.

fields(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"new Object()">>),
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

  FieldObj = ev8:eval(C, <<"new Object">>),
  ev8:set(C, Obj, FieldObj, <<"godzilla strikes">>),
  <<"godzilla strikes">> = ev8:get(C, Obj, FieldObj),

  ok.

multi_fields(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"new Object">>),
  FieldObj = ev8:eval(C, <<"new Object">>),

  [{ok, <<"fieldObj">>},
   {ok, <<"true">>},
   {ok, false},
   {error, bad_field}] = ev8:set(C, Obj, [{FieldObj, <<"fieldObj">>},
                                          {true, <<"true">>},
                                          {false, false},
                                          does_nothing]),

  <<"fieldObj">> = ev8:get(C, Obj, FieldObj),
  <<"true">> = ev8:get(C, Obj, true),
  false = ev8:get(C, Obj, false),
  undefined = ev8:get(C, Obj, does_nothing),

  ok.

set_no_wrap(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"new Object">>),

  [{ok, "hello"},
   {ok, {"true"}}] = ev8:set(C, Obj, [{<<"something">>, {"hello"}},
                                      {<<"else">>, {{"true"}}}]),

  ok.

wrapped_fun(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"var a = new Object(); a">>),
  ev8:set(C, Obj, <<"erlFun">>, fun(A, B) -> A + B end),

  6 = ev8:eval(C, <<"a.erlFun(2, 4)">>),

  ok.

call(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"new String('hello,world')">>),
  Fun = ev8:get(C, Obj, <<"split">>),
  ev8:set(C, Obj, <<"myFun">>, fun(A, B) -> A + B end),
  Fun2 = ev8:get(C, Obj, <<"myFun">>),
  Arr = ev8:call(C, Obj, Fun, [<<",">>]),

  <<"hello">> = ev8:get(C, Arr, 0),
  <<"world">> = ev8:get(C, Arr, 1),

  {error, badfun} = ev8:call(C, Fun, Obj, [<<",">>]),
  {error, badargs} = ev8:call(C, Obj, Fun, <<",">>),

  1337 = ev8:call(C, Fun2, [1330, 7]),

  ok.

global(Config) ->
  C = ?config(context, Config),

  ev8:set(C, global, <<"globalProp">>, 1337),
  1337 = ev8:eval(C, <<"globalProp">>),

  ok.

multi_context_call(Config)->
  Vm = ?config(vm, Config),
  C1 = ?config(context, Config),
  C2 = ev8:new_context(Vm),

  Obj = ev8:eval(C1, <<"new String('hello world')">>),
  Fun = ev8:get(C1, Obj, <<"toString">>),
  <<"hello world">> = ev8:call(C2, Obj, Fun, []),

  Fun2 = ev8:eval(C1, <<"var f = function() { return globalVal; }; f">>),
  ev8:set(C1, global, <<"globalVal">>, 42),
  ev8:set(C2, global, <<"globalVal">>, -42),
  42 = ev8:call(C1, Fun2, []),
  42 = ev8:call(C2, Fun2, []),

  C3 = ev8:new_context(Vm),
  ev8:set(C3, global, <<"anotherFun">>, fun() -> ev8:call(C2, Fun2, []) end),
  Fun3 = ev8:get(C3, global, <<"anotherFun">>),
  ev8:set(C1, global, <<"multiFun">>, fun() ->
        [ev8:eval(C2, <<"'crazy context'">>),
         ev8:call(C2, Fun2, []),
         ev8:call(C3, Fun3, [])]
    end),
  Fun4 = ev8:get(C1, global, <<"multiFun">>),
  [<<"crazy context">>, 42, 42] = ev8:call(C1, Fun4, []),

  ok.

arrays(Config) ->
  C = ?config(context, Config),

  ev8:set(C, global, <<"myArr">>, [<<"hello">>, true, false, 22]),
  <<"hello">> = ev8:eval(C, <<"myArr[0]">>),

  ok.
