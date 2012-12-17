
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
  V8ContextSpec = {v8context_sup,
                   {v8context_sup, start_link, []},
                   permanent, 5000, supervisor, [v8context_sup]},
  V8CallSpec = {v8call_sup,
                   {v8call_sup, start_link, []},
                   permanent, 5000, supervisor, [v8call_sup]},
  {ok, {{one_for_one, 5, 10}, [V8ContextSpec, V8CallSpec]}}.

