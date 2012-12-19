-module(erlang_v8).

-export([
  start/0,
  stop/0
  ]).

-spec start() -> ok | {error, Reason}
  when Reason :: term().
start() ->
  application:start(erlang_v8).

-spec stop() -> ok | {error, Reason}
  when Reason :: term().
stop() ->
  application:stop(erlang_v8).
