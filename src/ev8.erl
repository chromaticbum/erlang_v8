-module(ev8).

-include("erlang_v8.hrl").

-export([
  new_vm/0,
  set_vm_server/2,
  new_context/1,
  vm_for_context/1
  ]).

% VM Functions
-export([
  wrap/2,
  unwrap/2,
  eval_file/2,
  eval/2,
  eval/3,
  set/3,
  set/4,
  get/3,
  call/3,
  call/4,
  call_constructor/3,
  heap_statistics/1,
  call_respond/2,
  wrap_fun/2,
  install/2
  ]).

-export([
  execute_eval/4,
  execute_call/5,
  execute_call_constructor/4,
  execute_eval_file/3,
  execute/3
  ]).

-spec new_vm() -> v8nif:vm().
new_vm() ->
  Vm = v8nif:new_vm(),
  {ok, _Pid} = ev8vm_sup:create(Vm),
  Vm.

-spec set_vm_server(v8nif:vm(), pid()) -> ok.
set_vm_server(Vm, Server) ->
  v8nif:set_vm_server(Vm, Server).

-spec new_context(v8nif:vm()) -> v8nif:ev8_context().
new_context(Vm) ->
  v8nif:vm_execute(Vm, self(), {new_context}),
  Ctx = receive_result(),
  Ctx.

vm_for_context(Context) ->
  v8nif:vm_for_context(Context).

wrap(Context, Term) ->
  execute(Context, self(), {wrap, Term}).

unwrap(Context, Term) ->
  execute(Context, self(), {unwrap, Term}).

eval_file(Context, File) ->
  execute_eval_file(Context, File, 1).

eval(Context, Source) ->
  eval(Context, {<<"unknown">>, 0}, Source).

eval(Context, {File, Line}, Source) ->
  execute_eval(Context, {File, Line}, Source, 1).

set(Context, JsObject, FieldList) ->
  execute(Context, self(), {set, JsObject, FieldList}).

set(Context, JsObject, Field, Term) ->
  set(Context, JsObject, [{Field, Term}]).

get(Context, JsObject, Field) ->
  execute(Context, self(), {get, JsObject, Field, 1}).

call(Context, Fun, Args) ->
  call(Context, global, Fun, Args).

call(Context, Recv, Fun, Args) ->
  execute_call(Context, Recv, Fun, Args, 1).

call_constructor(Context, Fun, Args) ->
  execute_call_constructor(Context, Fun, Args, 1).

heap_statistics(Context) ->
  execute(Context, self(), {heap_statistics}).

call_respond(Context, Result) ->
  io:format("Call Respond(~p): ~p~p~n", [self(), Context, Result]),
  send_response(Context, Result).

wrap_fun(JsFun, method) ->
  fun(Context, Recv, Args) ->
      ev8:call(Context, Recv, JsFun, Args)
  end;
wrap_fun(JsFun, static) ->
  fun(Context, Args) ->
      ev8:call(Context, JsFun, Args)
  end.

install(Context, Plugins) ->
  lists:foreach(fun(Plugin) ->
        Plugin:install(Context)
    end, Plugins).

% Internal functions
send_response(Context, {error, Reason}) ->
  execute(Context, self(), {call_respond, {error, Reason}});
send_response(Context, Result) ->
  execute(Context, self(), {call_respond, {ok, Result}}).

execute_eval_file(Context, File, Wrap) ->
  execute_eval_file(Context, File, file:read_file(File), Wrap).

execute_eval_file(Context, File, {ok, Source}, Wrap) ->
  execute_eval(Context, {File, 0}, Source, Wrap);
execute_eval_file(_Context, _File, {error, Reason}, _Wrap) ->
  {error, Reason}.

execute_call_constructor(Context, Fun, Args, Wrap) ->
  execute(Context, self(), {call, constructor, {Fun, Args}, Wrap}).

execute_call(Context, Recv, Fun, Args, Wrap) ->
  execute(Context, self(), {call, normal, {Recv, Fun, Args}, Wrap}).

execute_eval(Context, {File, Line}, Source, Wrap) when is_list(File) ->
  io:format("Origin: ~p~n", [File]),
  execute_eval(Context, {list_to_binary(File), Line}, Source, Wrap);
execute_eval(Context, {File, Line}, Source, Wrap) ->
  execute(Context, self(), {eval, {File, Line}, Source, Wrap}).

execute(Context, Pid, Command) ->
  io:format("EXEC(~p): ~p~n", [Pid, Command]),
  v8nif:execute(Context, Pid, Command),
  receive_result().

receive_result() ->
  receive
    {ok, Result} -> Result;
    {error, Reason} -> {error, Reason}
  end.
