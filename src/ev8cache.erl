-module(ev8cache).

-export([
  start/0,
  eval_file/2
  ]).

start() ->
  ets:new(ev8cache_lookup, [set, public, named_table, {read_concurrency, true}]).

eval_file(Context, File) ->
  eval_file(ev8txn:vm(Context), Context, File).

eval_file({ok, Vm}, Context, File) ->
  try_cache(Vm, Context, File);
eval_file({error, Reason}, _Context, _File) -> {error, Reason}.

% Internal functions

try_cache(Vm, Context, File) ->
  case lookup(Vm, File) of
    {ok, Result} -> Result;
    {error, not_found} -> cache_miss(Vm, Context, File)
  end.

cache_miss(Vm, Context, File) ->
  case ev8:eval_file(Context, File) of
    {error, Reason} -> {error, Reason};
    Result ->
      insert(Vm, File, Result),
      Result
  end.

insert(Vm, File, Result) ->
  ets:insert(ev8cache_lookup, {{Vm, File}, Result}).

lookup(Vm, File) ->
  lookup(ets:lookup(ev8cache_lookup, {Vm, File})).

lookup([{{_Vm, _File}, Result}]) ->
  {ok, Result};
lookup([]) ->
  {error, not_found}.
