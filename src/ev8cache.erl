-module(ev8cache).

-export([
  start/0,
  eval_file/2,
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
  try_cache(Vm, Context, File).

try_cache(Vm, Context, File) ->
  try_cache(lookup(Vm, {eval_file, File}), Vm, Context, File).

try_cache({ok, Result}, _Vm, _Context, _File) ->
  Result;
try_cache(cache_miss, Vm, Context, File) ->
  Result = ev8:eval_file(Context, File),
  insert(Vm, {eval_file, File}, Result),
  Result.

insert(Vm, Key, Result) ->
  ets:insert(ev8cache_lookup, {{Vm, Key}, Result}).

lookup(Vm, Key) ->
  lookup(ets:lookup(ev8cache_lookup, {Vm, Key})).

lookup([{{_, _}, Result}]) ->
  {ok, Result};
lookup([]) ->
  cache_miss.
