-module(v8nif).
-on_load(init/0).

-export([
  init/0,
  new_vm/0,
  new_context/1,
  execute/3,
  define/3
  ]).

init() ->
  case code:priv_dir(erlang_v8) of
    {error, Reason} -> {error, Reason};
    Filename ->
      erlang:load_nif(filename:join([Filename, "erlang_v8_drv"]), 0)
  end.

new_vm() ->
  error(not_loaded).

new_context(_Vm) ->
  error(not_loaded).

execute(_Ctx, _Pid, _Js) ->
  error(not_loaded).

define(_JsWrapper, _Field, _Term) ->
  error(not_loaded).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

execute_test() -> ok.
  %I = new_isolate(),
  %C = new_context(I),

  %?assertMatch({js_array, _, _}, execute(C, <<"[]">>)),
  %?assertMatch({js_boolean_object, _, _}, execute(C, <<"new Boolean()">>)),
  %?assertMatch({js_date, _, _}, execute(C, <<"new Date()">>)),
  %?assertMatch({js_function, _, _}, execute(C, <<"var f = function() { }; f;">>)),
  %?assertMatch({js_number_object, _, _}, execute(C, <<"new Number()">>)),
  %?assertMatch({js_reg_exp, _, _}, execute(C, <<"/hey/">>)),
  %?assertMatch({js_string_object, _, _}, execute(C, <<"new String()">>)),
  %?assertMatch({js_undefined, _, _}, execute(C, <<"undefined">>)),
  %?assertMatch({js_null, _, _}, execute(C, <<"null">>)),
  %?assertMatch({js_number, _, _, 22}, execute(C, <<"22">>)),
  %?assertMatch({js_number, _, _, -22}, execute(C, <<"-22">>)),
  %?assertMatch({js_number, _, _, 22.2}, execute(C, <<"22.2">>)),
  %?assertMatch({js_boolean, _, _, true}, execute(C, <<"true">>)),
  %?assertMatch({js_string, _, _, <<"hello">>}, execute(C, <<"'hello'">>)),
  %?assertMatch({js_object, _, _}, execute(C, <<"new Object()">>)).

-endif.
