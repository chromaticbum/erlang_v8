-module(ev8).

-export([
  new_vm/0,
  new_context/1,
  set_context_server/2
  ]).

% VM Functions
-export([
  run_script/2,
  set/3,
  set/4,
  get/3,
  call/3,
  call/4,
  call_constructor/3,
  heap_statistics/1,
  call_respond/3
  ]).

new_vm() ->
  v8nif:new_vm().

new_context(Vm) ->
  Ctx = v8nif:new_context(Vm),
  {ok, _Pid} = v8context_srv:create(Ctx),
  Ctx.

set_context_server(Context, Server) ->
  v8nif:set_context_server(Context, Server).

set(Context, JsObject, FieldList) ->
  execute(Context, self(), {set, JsObject, FieldList}).

set(Context, JsObject, Field, Term) ->
  set(Context, JsObject, [{Field, Term}]).

get(Context, JsObject, Field) ->
  execute(Context, self(), {get, JsObject, Field}).

call(Context, Fun, Args) ->
  call(Context, undefined, Fun, Args).

call(Context, Recv, Fun, Args) ->
  execute(Context, self(), {call, normal, {Recv, Fun, Args}}).

call_constructor(Context, Fun, Args) ->
  execute(Context, self(), {call, constructor, {Fun, Args}}).

run_script(Context, Source) ->
  execute(Context, self(), {run_script, Source}).

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
