-module(ev8txn).

-export([
  start/0
  ]).

start() ->
  ets:new(ev8txn_lookup, [set, public, named_table, {read_concurrency, true}]).
