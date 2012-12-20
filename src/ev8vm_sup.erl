-module(ev8vm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_child(Context) ->
  supervisor:start_child(?MODULE, [Context]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  Ev8VmSpec = {ev8vm_srv,
                   {ev8vm_srv, start_link, []},
                   temporary, 5000, worker, [ev8vm_srv]},
  {ok, {{simple_one_for_one, 5, 10}, [Ev8VmSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
