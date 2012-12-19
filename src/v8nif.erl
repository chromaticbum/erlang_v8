-module(v8nif).
-on_load(init/0).

-export([
  init/0,
  new_vm/0,
  set_vm_server/2,
  new_context/1,
  execute/3
  ]).

-include("erlang_v8.hrl").

-spec init() -> ok | {error, Reason}
  when Reason :: term().
init() ->
  case code:priv_dir(erlang_v8) of
    {error, Reason} -> {error, Reason};
    Filename ->
      ok = erlang:load_nif(filename:join([Filename, "erlang_v8_drv"]), 0)
  end.

-spec new_vm() -> vm().
new_vm() ->
  error(not_loaded).

-spec set_vm_server(vm(), pid()) -> ok.
set_vm_server(_Vm, _Server) ->
  error(not_loaded).

-spec new_context(vm()) -> vm_context().
new_context(_Vm) ->
  error(not_loaded).

-spec execute(vm_context(), pid(), binary()) -> Result
  when Result :: atom()
  | binary()
  | float()
  | fun()
  | integer()
  | list().
execute(_Ctx, _Pid, _Command) ->
  error(not_loaded).
