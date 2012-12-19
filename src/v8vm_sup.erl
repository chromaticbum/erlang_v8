-module(v8vm_sup).

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
  V8VmSpec = {v8vm_srv,
                   {v8vm_srv, start_link, []},
                   temporary, 5000, worker, [v8vm_srv]},
  {ok, {{simple_one_for_one, 5, 10}, [V8VmSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
