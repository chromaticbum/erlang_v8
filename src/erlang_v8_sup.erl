
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
  V8VmSpec = {v8vm_sup,
                   {v8vm_sup, start_link, []},
                   permanent, 5000, supervisor, [v8vm_sup]},
  V8CallSpec = {v8call_sup,
                   {v8call_sup, start_link, []},
                   permanent, 5000, supervisor, [v8call_sup]},
  {ok, {{one_for_one, 5, 10}, [V8VmSpec, V8CallSpec]}}.

