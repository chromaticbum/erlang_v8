-module(ev8_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Vm) ->
  supervisor:start_child(?MODULE, [Vm]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  Ev8VmSupSpec = {ev8vm_sup,
                  {ev8vm_sup, start_link, []},
                  permanent, 5000, worker, [ev8vm_sup]},
  {ok, {{simple_one_for_one, 5, 10}, [Ev8VmSupSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

