-module(erlang_v8).

-export([
  start/0,
  stop/0
  ]).

start() ->
  application:start(erlang_v8).

stop() ->
  application:stop(erlang_v8).
