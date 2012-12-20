-module(ev8).

-export([
  new_vm/0,
  set_vm_server/2,
  new_context/1
  ]).

% VM Functions
-export([
  run_script/2,
  run_script/3,
  set/3,
  set/4,
  get/3,
  call/3,
  call/4,
  call_constructor/3,
  heap_statistics/1,
  call_respond/3
  ]).

-spec new_vm() -> v8nif:vm().
new_vm() ->
  Vm = v8nif:new_vm(),
  {ok, _Pid} = ev8vm_sup:create(Vm),
  Vm.

-spec set_vm_server(v8nif:vm(), pid()) -> ok.
set_vm_server(Vm, Server) ->
  v8nif:set_vm_server(Vm, Server).

-spec new_context(v8nif:vm()) -> v8nif:vm_context().
new_context(Vm) ->
  Ctx = v8nif:new_context(Vm),
  Ctx.

run_script(Context, Source) ->
  run_script(Context, {<<"unknown">>, 0}, Source).

run_script(Context, {File, Line}, Source) when is_list(File) ->
  run_script(Context, {list_to_binary(File), Line}, Source);
run_script(Context, {File, Line}, Source) ->
  io:format("file is: ~p:~p~n", [File, Line]),
  execute(Context, self(), {run_script, {File, Line}, Source}).

set(Context, JsObject, FieldList) ->
  execute(Context, self(), {set, JsObject, FieldList}).

set(Context, JsObject, Field, Term) ->
  set(Context, JsObject, [{Field, Term}]).

get(Context, JsObject, Field) ->
  execute(Context, self(), {get, JsObject, Field}).

call(Context, Fun, Args) ->
  call(Context, global, Fun, Args).

call(Context, Recv, Fun, Args) ->
  execute(Context, self(), {call, normal, {Recv, Fun, Args}}).

call_constructor(Context, Fun, Args) ->
  execute(Context, self(), {call, constructor, {Fun, Args}}).

heap_statistics(Context) ->
  execute(Context, self(), {heap_statistics}).

call_respond(Context, Fun, Args) ->
  Result = make_call(Fun, Args),
  io:format("Call Respond: ~p(~p) -> ~p~n", [Fun, Args, Result]),
  send_response(Context, Result).

send_response(Context, {error, Reason}) ->
  v8nif:execute(Context, self(), {call_respond, {error, Reason}});
send_response(Context, Result) ->
  v8nif:execute(Context, self(), {call_respond, {ok, Result}}).

make_call({Module, Fun}, Args) ->
  apply(Module, Fun, Args);
make_call(Fun, Args) ->
  apply(Fun, Args).

execute(Context, Pid, Command) ->
  v8nif:execute(Context, Pid, Command),
  receive_result().

receive_result() ->
  receive
    {ok, Result} -> Result;
    {error, Reason} -> {error, Reason}
  end.
