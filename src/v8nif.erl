-module(v8nif).
-on_load(init/0).

-export([
  init/0,
  new_vm/0,
  new_context/1,
  set_context_server/2,
  execute/3,
  set_field/3
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

set_context_server(_Ctx, _Server) ->
  error(not_loaded).

execute(_Ctx, _Pid, _Command) ->
  error(not_loaded).

set_field(_JsWrapper, _Field, _Term) ->
  error(not_loaded).
