-module(ev8vm_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         create/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Vm) ->
  supervisor:start_link(?MODULE, [Vm]).

create(Vm) ->
  ev8_sup:start_child(Vm).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Vm]) ->
  Ev8VmSrvSpec = {ev8vm_srv,
                  {ev8vm_srv, start_link, [Vm]},
                  permanent, 5000, worker, [ev8vm_srv]},
  Ev8TxnSrvSpec = {ev8txn_srv,
                   {ev8txn_srv, start_link, []},
                   permanent, 5000, worker, [ev8txn_srv]},
  {ok, {{one_for_all, 5, 10}, [Ev8VmSrvSpec, Ev8TxnSrvSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
