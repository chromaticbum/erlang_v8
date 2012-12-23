-module(evo8).

-export([
  eval_file/2,
  eval/2,
  eval/3,
  get/3,
  call/3,
  call/4,
  call_constructor/3,
  wrap_fun/2]).

eval_file(Context, File) ->
  ev8:execute_eval_file(Context, File, 0).

eval(Context, Source) ->
  eval(Context, {<<"unknown">>, 0}, Source).

eval(Context, {File, Line}, Source) ->
  ev8:execute_eval(Context, {File, Line}, Source, 0).

get(Context, JsObject, Field) ->
  ev8:execute(Context, self(), {get, JsObject, Field, 0}).

call(Context, Fun, Args) ->
  call(Context, global, Fun, Args).

call(Context, Recv, Fun, Args) ->
  ev8:execute_call(Context, Recv, Fun, Args, 0).

call_constructor(Context, Fun, Args) ->
  ev8:execute_call_constructor(Context, Fun, Args, 0).

wrap_fun(JsFun, method) ->
  fun(Context, Recv, Args) ->
      evo8:call(Context, Recv, JsFun, Args)
  end;
wrap_fun(JsFun, static) ->
  fun(Context, Args) ->
      evo8:call(Context, JsFun, Args)
  end.
