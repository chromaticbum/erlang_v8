-module(transaction_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
  ]).

-export([
  no_txn/1,
  with_txn/1
  ]).

all() ->
  [no_txn,
  with_txn].

init_per_suite(Config) ->
  erlang_v8:start(),
  Vm = ev8:new_vm(),
  C1 = ev8:new_context(Vm),
  C2 = ev8:new_context(Vm),
  [{vm, Vm}, {c1, C1}, {c2, C2} | Config].

end_per_suite(Config) ->
  erlang_v8:stop(),
  Config.

% Illustrates the issue with not running your code in a transaction
no_txn(Config) ->
  C1 = ?config(c1, Config),
  C2 = ?config(c2, Config),

  Obj = ev8:eval(C1, <<"new Object">>),
  ev8:set(C1, Obj, <<"longFun">>, fun(_This) ->
        timer:sleep(200), <<"long">> end),
  ev8:set(C1, Obj, <<"shortFun">>, fun(_This) ->
        timer:sleep(100), <<"short">> end),

  LongFun = ev8:get(C1, Obj, <<"longFun">>),
  ShortFun = ev8:get(C1, Obj, <<"shortFun">>),

  Self = self(),
  spawn(fun() ->
        Result = evo8:call(C1, ShortFun, []),
        Self ! {short, Result}
    end),
  spawn(fun() ->
        Result = evo8:call(C2, LongFun, []),
        Self ! {long, Result}
    end),

  ok = receive
    {long, <<"short">>} -> ok;
    {long, _} -> {error, wrong_return}
  end,

  ok = receive
    {short, <<"long">>} -> ok;
    {short, _} -> {error, wrong_return}
  end,

  ok.

with_txn(Config) ->
  C1 = ?config(c1, Config),
  C2 = ?config(c2, Config),

  Obj = ev8:eval(C1, <<"new Object">>),
  ev8:set(C1, Obj, <<"longFun">>, fun(_This) ->
        timer:sleep(200), <<"long">> end),
  ev8:set(C1, Obj, <<"shortFun">>, fun(_This) ->
        timer:sleep(100), <<"short">> end),

  LongFun = ev8:get(C1, Obj, <<"longFun">>),
  ShortFun = ev8:get(C1, Obj, <<"shortFun">>),

  Self = self(),
  spawn(fun() ->
        Result = ev8:transaction(C1, fun() ->
                evo8:call(C1, ShortFun, [])
            end),
        Self ! {short, Result}
    end),
  spawn(fun() ->
        Result = ev8:transaction(C2, fun() ->
                evo8:call(C2, LongFun, [])
            end),
        Self ! {long, Result}
    end),

  ok = receive
    {long, {atomic, <<"long">>}} -> ok;
    {long, _} -> {error, wrong_return}
  end,

  ok = receive
    {short, {atomic, <<"short">>}} -> ok;
    {short, _} -> {error, wrong_return}
  end,

  ok.
