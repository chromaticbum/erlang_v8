-module(v8call_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Context) ->
  supervisor:start_child(?MODULE, [Context]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  V8CallSrvSpec = {v8call_srv,
                   {v8call_srv, start_link, []},
                   temporary, 5000, worker, [v8call_srv]},
  {ok, {{simple_one_for_one, 5, 10}, [V8CallSrvSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
