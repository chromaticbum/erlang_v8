-module(ev8_erlang).

-export([
  install/1
  ]).

install(C) ->
  Fun = fun() ->
      Obj = ev8:eval(C, <<"new Object">>),
      ev8:set(C, global, <<"erlang">>, Obj),
      ev8:set(C, Obj, [{<<"string_to_atom">>, fun string_to_atom/2},
                       {<<"string_to_list">>, fun string_to_list/2},
                       {<<"atom_to_string">>, fun atom_to_string/2},
                       {<<"list_to_string">>, fun list_to_string/2},
                       {<<"array_to_list">>, fun array_to_list/2},
                       {<<"array_to_tuple">>, fun array_to_tuple/2},
                       {<<"object_to_proplist">>, fun object_to_proplist/2},
                       {<<"_apply">>, fun js_apply/4}]),
      ErlangJs = filename:absname(filename:join(code:priv_dir(erlang_v8), "js/ev8_erlang.js")),
      ev8:eval_file(C, ErlangJs),
      ok
  end,
  {atomic, ok} = ev8:transaction(C, Fun).

% Internal functions

string_to_atom(_This, String) when is_binary(String) ->
  {list_to_atom(binary_to_list(String))}.

string_to_list(_This, String) when is_binary(String) ->
  {binary_to_list(String)}.

atom_to_string(_This, Atom) when is_atom(Atom) ->
  list_to_binary(atom_to_list(Atom)).

list_to_string(_This, List) when is_list(List) ->
  list_to_binary(List).

array_to_list(_This, Arr) when is_list(Arr) ->
  {Arr}.

array_to_tuple(_This, Arr) when is_list(Arr) ->
  {list_to_tuple(Arr)}.

object_to_proplist(_This, {struct, PropList}) when is_list(PropList) ->
  {PropList}.

js_apply(This, Module, Fun, Args) when is_binary(Module) ->
  js_apply(This, list_to_atom(binary_to_list(Module)), Fun, Args);
js_apply(This, Module, Fun, Args) when is_binary(Fun) ->
  js_apply(This, Module, list_to_atom(binary_to_list(Fun)), Args);
js_apply(_This, Module, Fun, Args) ->
  io:format("Call: ~p:~p with ~p~n", [Module, Fun, Args]),
  {apply(Module, Fun, Args)}.
