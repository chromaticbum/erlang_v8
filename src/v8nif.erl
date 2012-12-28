-module(v8nif).
-on_load(init/0).

-export([
  init/0,
  new_vm/0,
  set_vm_server/2,
  execute/3,
  vm_execute/3,
  vm_for_context/1
  ]).

-export_type([
    vm/0,
    vm_context/0,
    command/0,
    cmd_heap_statistics/0,
    cmd_call_respond/0,
    cmd_eval/0,
    cmd_set/0,
    cmd_get/0
    ]).

-type vm() :: binary().
-type vm_context() :: binary().

-type cmd_heap_statistics() :: {heap_statistics}.

-type call_type() :: normal | constructor.
-type js_fun() :: binary() | fun().
-type js_obj() :: binary() | global.
-type args() :: [any()].
-type call() :: {js_fun(), args()} |
  {js_obj(), js_fun(), args()}.
-type cmd_call() :: {call, call_type(), call()}.

-type cmd_get() :: {get, js_obj(), any()}.

-type field_set() :: {any(), any()}.
-type cmd_set() :: {set, js_obj(), [field_set()]}.

-type cmd_eval() :: {eval, {binary(), integer()}, iolist()}.

-type call_response() :: {ok, any()} | {error, any()}.
-type cmd_call_respond() :: {call_respond, call_response()}.

-type command() :: cmd_heap_statistics() |
  cmd_call() | cmd_get() | cmd_set() |
  cmd_eval() | cmd_call_respond().

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
  erlang:nif_error(not_loaded).

-spec set_vm_server(vm(), pid()) -> ok.
set_vm_server(_Vm, _Server) ->
  erlang:nif_error(not_loaded).

-spec execute(vm_context(), pid(), command()) -> Result
  when Result :: any().
execute(_Ctx, _Pid, _Command) ->
  erlang:nif_error(not_loaded).

-spec vm_execute(vm(), pid(), any()) -> ok.
vm_execute(_Ctx, _Pid, _Command) ->
  elrang:nif_error(not_loaded).

-spec vm_for_context(vm_context()) -> vm().
vm_for_context(_Ctx) ->
  erlang:nif_error(not_loaded).
