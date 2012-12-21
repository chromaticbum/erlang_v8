-module(ev8_erlang).

-export([
  install/1
  ]).

install(C) ->
  Fun = fun() ->
      Obj = ev8:eval_wrapped(C, <<"new Object">>),
      ev8:set(C, global, <<"erlang">>, Obj),
      ev8:set(C, Obj, [{<<"string_to_atom">>, fun string_to_atom/1},
                       {<<"string_to_list">>, fun string_to_list/1},
                       {<<"array_to_list">>, fun array_to_list/1},
                       {<<"array_to_tuple">>, fun array_to_tuple/1},
                       {<<"object_to_proplist">>, fun object_to_proplist/1},
                       {<<"_apply">>, fun js_apply/3}]),
      ErlangJs = filename:absname(filename:join(code:priv_dir(erlang_v8), "js/ev8_erlang.js")),
      ev8:eval(C, {ErlangJs, 0}, file:read_file(ErlangJs)),
      ok
  end,
  {atomic, ok} = ev8:transaction(C, Fun).

% Internal functions

string_to_atom(String) when is_binary(String) ->
  {list_to_atom(binary_to_list(String))}.

string_to_list(String) when is_binary(String) ->
  {binary_to_list(String)}.

array_to_list(Arr) when is_list(Arr) ->
  {Arr}.

array_to_tuple(Arr) when is_list(Arr) ->
  {list_to_tuple(Arr)}.

object_to_proplist({struct, PropList}) when is_list(PropList) ->
  {PropList}.

js_apply(Module, Fun, Args) when is_binary(Module) ->
  js_apply(list_to_atom(binary_to_list(Module)), Fun, Args);
js_apply(Module, Fun, Args) when is_binary(Fun) ->
  js_apply(Module, list_to_atom(binary_to_list(Fun)), Args);
js_apply(Module, Fun, Args) ->
  io:format("Call: ~p:~p with ~p~n", [Module, Fun, Args]),
  {apply(Module, Fun, Args)}.
