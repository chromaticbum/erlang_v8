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
