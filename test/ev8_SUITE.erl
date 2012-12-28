-module(ev8_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  test_fun/2
  ]).

-export([
  eval_file/1,
  eval/1,
  script_origin/1,
  fields/1,
  multi_fields/1,
  set_no_wrap/1,
  wrapped_fun/1,
  wrap_fun/1,
  call/1,
  global/1,
  multi_context_call/1,
  arrays/1,
  objects/1,
  ev8__/1
  ]).

all() ->
  [eval_file,
   eval,
   script_origin,
   fields,
   set_no_wrap,
   multi_fields,
   wrapped_fun,
   wrap_fun,
   call,
   global,
   multi_context_call,
   arrays,
   objects,
   ev8__].

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

  {struct, [{<<"that">>, <<"is">>},
            {<<"a">>, <<"file">>}]} = evo8:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/eval_file.js")),
  <<>> = ev8:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/eval_file.js")),

  {error, {js_error, _, StackTrace}} = evo8:eval_file(C, filename:join(code:lib_dir(erlang_v8), "test/ev8/throw_error.js")),
  true = string:str(binary_to_list(StackTrace), "throw_error.js:0:7") > 0,

   ok.

eval(Config) ->
  C = ?config(context, Config),

  undefined = evo8:eval(C, <<"undefined">>),
  null = evo8:eval(C, <<"null">>),
  22 = evo8:eval(C, <<"22">>),
  -22 = evo8:eval(C, <<"-22">>),
  22.2 = evo8:eval(C, <<"22.2">>),
  true = evo8:eval(C, <<"true">>),
  false = evo8:eval(C, <<"false">>),
  <<"hello">> = evo8:eval(C, <<"'hello'">>),
  {struct, []} = evo8:eval(C, <<"new Object()">>),
  <<>> = ev8:eval(C, <<"false">>),
  <<>> = ev8:eval(C, <<"new Object()">>),

  {error, {js_error, _Info, _StackTrace}} = evo8:eval(C, <<"i.myFun()">>),

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
  ev8:set(C, Obj, <<"erlWrapped">>, true),

  undefined = evo8:get(C, Obj, <<"erlUndefined">>),
  null = evo8:get(C, Obj, <<"erlNull">>),
  22 = evo8:get(C, Obj, <<"erlInt">>),
  -22 = evo8:get(C, Obj, <<"erlNegInt">>),
  22.2 = evo8:get(C, Obj, <<"erlDouble">>),
  true = evo8:get(C, Obj, <<"erlTrue">>),
  false = evo8:get(C, Obj, <<"erlFalse">>),
  <<"godzilla strikes">> = evo8:get(C, Obj, <<"erlBinary">>),
  [<<"hello">>, <<"there">>, [true, false, null, undefined]] = evo8:get(C, Obj, <<"erlList">>),
  true = evo8:get(C, Obj, <<"erlWrapped">>),
  <<>> = ev8:get(C, Obj, <<"erlWrapped">>),

  {error, invalid_object} = (catch evo8:get(C, 2, <<"heyThere">>)),
  {error, invalid_object} = (catch ev8:set(C, <<"godzilla">>, <<"heyThere">>, <<"dude">>)),

  FieldObj = ev8:eval(C, <<"new Object">>),
  ev8:set(C, Obj, FieldObj, <<"godzilla strikes">>),
  <<"godzilla strikes">> = evo8:get(C, Obj, FieldObj),

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

  <<"fieldObj">> = evo8:get(C, Obj, FieldObj),
  <<"true">> = evo8:get(C, Obj, true),
  false = evo8:get(C, Obj, false),
  undefined = evo8:get(C, Obj, does_nothing),

  ok.

set_no_wrap(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"new Object">>),

  [{ok, "hello"},
   {ok, {"true"}}] = ev8:set(C, Obj, [{<<"something">>, {"hello"}},
                                      {<<"else">>, {{"true"}}}]),

  ok.

test_fun(_, []) ->
  <<"test_funn">>.

wrapped_fun(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"var a = new Object(); a">>),
  ev8:set(C, Obj, <<"erlFun">>, fun(_, [A, B]) -> A + B end),

  6 = evo8:eval(C, <<"a.erlFun(2, 4)">>),

  ev8:set(C, global, <<"testFun">>, {mf, {?MODULE, test_fun}}),
  <<"test_funn">> = evo8:eval(C, <<"testFun()">>),

  ok.

wrap_fun(Config) ->
  C = ?config(context, Config),

  ev8:eval(C, <<"function myFun(a, b) { return a + b; }">>),
  Obj = ev8:eval(C, <<"new String('hello world')">>),
  Split = ev8:get(C, Obj, <<"split">>),
  Fun = ev8:get(C, global, <<"myFun">>),

  Wrapped = evo8:wrap_fun(Fun, static),
  42 = Wrapped(C, [40, 2]),

  SplitWrapped = evo8:wrap_fun(Split, method),
  [<<"hello">>, <<"world">>] = SplitWrapped(C, Obj, [<<" ">>]),
  [<<"hello">>, <<"godzilla">>] = SplitWrapped(C, <<"hello godzilla">>, [<<" ">>]),

  ok.

call(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"new String('hello,world')">>),
  Fun = ev8:get(C, Obj, <<"split">>),
  ev8:set(C, Obj, <<"myFun">>, fun(_This, [A, B]) -> A + B end),
  Fun2 = ev8:get(C, Obj, <<"myFun">>),
  [<<"hello">>, <<"world">>] = evo8:call(C, Obj, Fun, [<<",">>]),

  <<>> = ev8:call(C, Obj, Fun, [<<",">>]),

  {error, badfun} = ev8:call(C, Fun, Obj, [<<",">>]),
  {error, badargs} = ev8:call(C, Obj, Fun, <<",">>),

  1337 = evo8:call(C, Fun2, [1330, 7]),

  ok.

global(Config) ->
  C = ?config(context, Config),

  ev8:set(C, global, <<"globalProp">>, 1337),
  1337 = evo8:eval(C, <<"globalProp">>),

  ok.

multi_context_call(Config)->
  Vm = ?config(vm, Config),
  C1 = ?config(context, Config),
  C2 = ev8:new_context(Vm),

  Obj = ev8:eval(C1, <<"new String('hello world')">>),
  Fun = ev8:get(C1, Obj, <<"toString">>),
  <<"hello world">> = evo8:call(C2, Obj, Fun, []),

  Fun2 = ev8:eval(C1, <<"var f = function() { return globalVal; }; f">>),
  ev8:set(C1, global, <<"globalVal">>, 42),
  ev8:set(C2, global, <<"globalVal">>, -42),
  42 = evo8:call(C1, Fun2, []),
  42 = evo8:call(C2, Fun2, []),

  C3 = ev8:new_context(Vm),
  ev8:set(C3, global, <<"anotherFun">>, fun(_, []) -> ev8:call(C2, Fun2, []) end),
  Fun3 = ev8:get(C3, global, <<"anotherFun">>),
  ev8:set(C1, global, <<"multiFun">>, fun(_, []) ->
        [ev8:eval(C2, <<"'crazy context'">>),
         ev8:call(C2, Fun2, []),
         ev8:call(C3, Fun3, [])]
    end),
  Fun4 = ev8:get(C1, global, <<"multiFun">>),
  [<<"crazy context">>, 42, 42] = evo8:call(C1, Fun4, []),

  ok.

arrays(Config) ->
  C = ?config(context, Config),

  ev8:set(C, global, <<"myArr">>, [<<"hello">>, true, false, 22]),
  <<"hello">> = evo8:eval(C, <<"myArr[0]">>),

  ok.

objects(Config) ->
  C = ?config(context, Config),

  Obj = ev8:eval(C, <<"var a = new Object; a">>),
  ev8:set(C, Obj, [{<<"field1">>, <<"godzilla">>},
                   {<<"field2">>, <<"mothra">>}]),

  {struct, [{<<"field1">>, <<"godzilla">>},
            {<<"field2">>, <<"mothra">>}]} = evo8:eval(C, <<"a">>),

  ev8:set(C, global, <<"godzilla">>, {struct, [{<<"godzilla">>, <<"rocks">>}]}),
  {struct, [{<<"godzilla">>, <<"rocks">>}]} = evo8:eval(C, <<"godzilla">>),
  <<"rocks">> = evo8:eval(C, <<"godzilla.godzilla">>),

  ok.

ev8__(Config) ->
  Vm = ?config(vm, Config),
  C = ?config(context, Config),

  Vm = evo8:eval(C, <<"__ev8__.vm">>),
  Vm2 = evo8:eval(C, <<"__ev8__.vm">>),
  ev8:new_context(Vm2),
  C = evo8:eval(C, <<"__ev8__.context">>),
  <<"js/test.erl">> = evo8:eval(C, {"js/test.erl", 0}, <<"__ev8__.script_name">>),
  ev8:set(C, global, <<"scriptFun">>, fun(_, [])->
        evo8:eval(C, {"js/another.erl", 0}, <<"__ev8__.script_name">>)
    end),
  <<"js/another.erl">> = evo8:eval(C, {"js/test.erl", 0}, <<"scriptFun()">>),

  ok.
