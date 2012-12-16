-module(v8nif).
-on_load(init/0).

-export([
  init/0,
  new_vm/0,
  new_context/1,
  set_context_server/2,
  execute/3,
  heap_statistics/1
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

-spec new_context(vm()) -> vm_context().
new_context(_Vm) ->
  error(not_loaded).

-spec set_context_server(vm_context(), pid()) -> ok.
set_context_server(_Ctx, _Server) ->
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

heap_statistics(_Ctx) ->
  error(not_loaded).
