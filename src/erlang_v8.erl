-module(erlang_v8).

-export([
  execute_script/2
  ]).

execute_script(Context, Source) ->
  ResultPid = self(),
  Pid = spawn(fun() ->
          receive
            {result, Result} ->
              ResultPid ! {ok, Result}
          end
      end),
  v8nif:execute(Context, Pid, Source),
  receive
    {ok, Result} ->
      Result
  end.
