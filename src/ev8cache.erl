-module(ev8cache).

-export([
  start/0,
  eval_file/2,
  try_cache/3,
  insert/3,
  lookup/2
  ]).

start() ->
  ets:new(ev8cache_lookup, [set, public, named_table, {read_concurrency, true}]).

eval_file(Context, File) ->
  eval_file(ev8txn:vm(Context), Context, File).

eval_file({error, not_found}, _Context, _File) ->
  {error, badcontext};
eval_file({ok, Vm}, Context, File) ->
  Fun = fun() ->
      ev8:eval_file(Context, File)
  end,
  try_cache(Vm, {eval_file, File}, Fun).

try_cache(Vm, Key, Fun) ->
  try_cache(lookup(Vm, Key), Vm, Key, Fun).

try_cache({ok, Result}, _Vm, _Key, _Fun) ->
  Result;
try_cache(cache_miss, Vm, Key, Fun) ->
  Result = Fun(),
  insert(Vm, Key, Result),

  Result.

insert(Vm, Key, Result) ->
  ets:insert(ev8cache_lookup, {{Vm, Key}, Result}).

lookup(Vm, Key) ->
  lookup(ets:lookup(ev8cache_lookup, {Vm, Key})).

lookup([{{_, _}, Result}]) ->
  {ok, Result};
lookup([]) ->
  cache_miss.
