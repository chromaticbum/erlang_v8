-module(ev8txn_srv).

-behaviour(gen_server).

%% API
-export([start_link/1,
        transaction/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    vm}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Vm) ->
  gen_server:start_link(?MODULE, [Vm], []).

transaction(Pid, Fun) ->
  gen_server:call(Pid, {transaction, Fun}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Vm]) ->
  ev8txn:add_vm(Vm, self()),
  {ok, #state{
      vm = Vm}}.

handle_call({transaction, Fun}, _From, State) ->
  {reply, {atomic, Fun()}, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
