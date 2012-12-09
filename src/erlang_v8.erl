-module(erlang_v8).

-export([
  execute_script/2
  ]).

execute_script(Context, Source) ->
  ResultPid = self(),
  Pid = spawn(fun() ->
          receive
            {result, Result} ->
              io:format("Received result ~p~n", [Result]),
              ResultPid ! {ok, Result}
          end
      end),
  v8nif:execute(Context, Pid, Source),
  receive
    {ok, Result} ->
      io:format("Result PID received result ~p~n", [Result]),
      Result
  end.
