-module(ev8).

-export([
  start/0,
  new_vm/0,
  new_context/1,
  execute_script/2,
  call_respond/2
  ]).

start() ->
  application:start(erlang_v8).

new_vm() ->
  v8nif:new_vm().

new_context(Vm) ->
  {ok, Pid} = v8context_srv:create(),
  Ctx = v8nif:new_context(Vm, Pid),
  ok = v8context_srv:set_context(Pid, Ctx),
  Ctx.

execute_script(Context, Source) ->
  v8nif:execute(Context, self(), {script, Source}),
  receive
    {result, Result} ->
      Result
  end.

call_respond(Context, Result) ->
  v8nif:call_respond(Context, Result).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

execute_script_test() ->
  ok = application:start(erlang_v8),
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
