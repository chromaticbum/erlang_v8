-module(ev8).

-export([
  start/0,
  new_vm/0,
  new_context/1,
  execute_script/2
  ]).

start() ->
  application:start(erlang_v8).

new_vm() ->
  v8nif:new_vm().

new_context(Vm) ->
  {ok, Pid} = v8context_srv:create(),
  v8nif:new_context(Vm, Pid).

execute_script(Context, Source) ->
  io:format("Execute Script~n"),
  {ok, Pid} = v8call_srv:create(self()),
  v8nif:execute(Context, Pid, Source),
  receive
    {ok, Result} ->
      v8call_srv:stop(Pid),
      Result
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

execute_script_test() ->
  ok = application:start(erlang_v8),
  Vm = new_vm(),
  Ctx = new_context(Vm),

  ?assertMatch({js_array, _, _}, execute_script(Ctx, <<"[]">>)),
  ?assertMatch({js_boolean_object, _, _}, execute_script(Ctx, <<"new Boolean()">>)),
  ?assertMatch({js_date, _, _}, execute_script(Ctx, <<"new Date()">>)),
  ?assertMatch({js_function, _, _}, execute_script(Ctx, <<"var f = function() { }; f;">>)),
  ?assertMatch({js_number_object, _, _}, execute_script(Ctx, <<"new Number()">>)),
  ?assertMatch({js_reg_exp, _, _}, execute_script(Ctx, <<"/hey/">>)),
  ?assertMatch({js_string_object, _, _}, execute_script(Ctx, <<"new String()">>)),
  ?assertMatch({js_undefined, _, _}, execute_script(Ctx, <<"undefined">>)),
  ?assertMatch({js_null, _, _}, execute_script(Ctx, <<"null">>)),
  ?assertMatch({js_number, _, _, 22}, execute_script(Ctx, <<"22">>)),
  ?assertMatch({js_number, _, _, -22}, execute_script(Ctx, <<"-22">>)),
  ?assertMatch({js_number, _, _, 22.2}, execute_script(Ctx, <<"22.2">>)),
  ?assertMatch({js_boolean, _, _, true}, execute_script(Ctx, <<"true">>)),
  ?assertMatch({js_string, _, _, <<"hello">>}, execute_script(Ctx, <<"'hello'">>)),
  ?assertMatch({js_object, _, _}, execute_script(Ctx, <<"new Object()">>)).

-endif.
