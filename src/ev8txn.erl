-module(ev8txn).

-export([
  start/0,
  add_vm/2,
  add_context/2,
  transaction/2
  ]).

start() ->
  ets:new(ev8txn_lookup, [set, public, named_table, {read_concurrency, true}]).

add_vm(Vm, Pid) ->
  ets:insert(ev8txn_lookup, {{vm, Vm}, Pid}).

add_context(Vm, Context) ->
  ets:insert(ev8txn_lookup, {{context, Context}, Vm}).

transaction({ok, Pid}, Fun) ->
  ev8txn_srv:transaction(Pid, Fun);
transaction({error, not_found}, _Fun) ->
  {error, badcontext};
transaction(Context, Fun) ->
  transaction(context_server(Context), Fun).

% Internal functions
vm_server([]) ->
  {error, not_found};
vm_server([{{vm, _Vm}, Pid}]) ->
  {ok, Pid};
vm_server(Vm) ->
  vm_server(ets:lookup(ev8txn_lookup, {vm, Vm})).

context_server([]) ->
  {error, not_found};
context_server([{{context, _Context}, Vm}]) ->
  vm_server(Vm);
context_server(Context) ->
  context_server(ets:lookup(ev8txn_lookup, {context, Context})).
