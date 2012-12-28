-module(erlang_v8_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ev8cache:start(),
  erlang_v8_sup:start_link().

stop(_State) ->
  ok.
