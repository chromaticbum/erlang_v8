
-module(erlang_v8_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  V8SrvSpec = {erlang_v8_srv,
               {erlang_v8_srv, start_link, []},
               transient, 5000, worker, [erlang_v8_srv]},
  {ok, {{simple_one_for_one, 5, 10}, [V8SrvSpec]}}.

