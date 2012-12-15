-module(ev8).

-export([
  start/0,
  new_vm/0,
  new_context/1,
  set_context_server/2,
  execute_script/2,
  set_field/3,
  execute_field/4,
  call_respond/3
  ]).

start() ->
  application:start(erlang_v8).

new_vm() ->
  v8nif:new_vm().

new_context(Vm) ->
  Ctx = v8nif:new_context(Vm),
  {ok, _Pid} = v8context_srv:create(Ctx),
  Ctx.

set_context_server(Context, Server) ->
  v8nif:set_context_server(Context, Server).

set_field(JsObject, Field, Term) ->
  v8nif:set_field(JsObject, Field, Term).

execute_field(Context, JsObject, Field, Args) ->
  v8nif:execute(Context, self(), {call, JsObject, Field, Args}),
  receive
    {result, Result} ->
      Result
  end.

execute_script(Context, Source) ->
  v8nif:execute(Context, self(), {script, Source}),
  receive
    {result, Result} ->
      Result
  end.

call_respond(Context, Fun, Args) ->
  Result = make_call(Fun, Args),
  io:format("Call Respond: ~p(~p) -> ~p~n", [Fun, Args, Result]),
  v8nif:execute(Context, self(), {call_respond, Result}).

make_call({Module, Fun}, Args) ->
  apply(Module, Fun, Args);
make_call(Fun, Args) ->
  apply(Fun, Args).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

execute_object_test() ->
  application:start(erlang_v8),
  Vm = new_vm(),
  Ctx = new_context(Vm),

  Obj = execute_script(Ctx, <<"new Boolean(true)">>),
  Obj2 = execute_script(Ctx, <<"new Boolean(false)">>),
  Obj3 = execute_script(Ctx, <<"new String('hello world!')">>),
  ?assertMatch(true, execute_field(Ctx, Obj, <<"valueOf">>, null)),
  ?assertMatch(false, execute_field(Ctx, Obj2, <<"valueOf">>, null)),
  ?assertMatch(<<"hello world!">>, execute_field(Ctx, Obj3, <<"toString">>, null)).

execute_script_test() ->
  application:start(erlang_v8),
  Vm = new_vm(),
  Ctx = new_context(Vm),

  ?assertMatch(undefined, execute_script(Ctx, <<"undefined">>)),
  ?assertMatch(null, execute_script(Ctx, <<"null">>)),
  ?assertMatch(22, execute_script(Ctx, <<"22">>)),
  ?assertMatch(-22, execute_script(Ctx, <<"-22">>)),
  ?assertMatch(22.2, execute_script(Ctx, <<"22.2">>)),
  ?assertMatch(true, execute_script(Ctx, <<"true">>)),
  ?assertMatch(false, execute_script(Ctx, <<"false">>)),
  ?assertMatch(<<"hello">>, execute_script(Ctx, <<"'hello'">>)),
  ?assertMatch(<<>>, execute_script(Ctx, <<"new Object()">>)).

-endif.
