-module(ev8).

-export([
  start/0,
  stop/0,
  new_vm/0,
  new_context/1,
  set_context_server/2,
  run_script/2,
  set/4,
  get_field/3,
  execute_field/4,
  to_term/2,
  heap_statistics/1,
  call_respond/3
  ]).

start() ->
  application:start(erlang_v8).

stop() ->
  application:stop(erlang_v8).

new_vm() ->
  v8nif:new_vm().

new_context(Vm) ->
  Ctx = v8nif:new_context(Vm),
  {ok, _Pid} = v8context_srv:create(Ctx),
  Ctx.

set_context_server(Context, Server) ->
  v8nif:set_context_server(Context, Server).

set(Context, JsObject, Field, Term) ->
  execute(Context, self(), {set, JsObject, Field, Term}).

get_field(Context, JsObject, Field) ->
  execute(Context, self(), {get_field, JsObject, Field}).

execute_field(Context, JsObject, Field, Args) ->
  execute(Context, self(), {call, JsObject, Field, Args}).

run_script(Context, Source) ->
  execute(Context, self(), {run_script, Source}).

to_term(Context, JsObject) ->
  execute(Context, self(), {erl_native, JsObject}).

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
    {result, Result} -> Result
  end.
