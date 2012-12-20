
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
  Ev8VmSpec = {ev8vm_sup,
                   {ev8vm_sup, start_link, []},
                   permanent, 5000, supervisor, [ev8vm_sup]},
  Ev8CallSpec = {ev8call_sup,
                   {ev8call_sup, start_link, []},
                   permanent, 5000, supervisor, [ev8call_sup]},
  {ok, {{one_for_one, 5, 10}, [Ev8VmSpec, Ev8CallSpec]}}.

