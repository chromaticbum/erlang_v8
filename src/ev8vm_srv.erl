-module(ev8vm_srv).

-behaviour(gen_server).

%% API
-export([start_link/1,
         create/1]).

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

create(Vm) ->
  ev8vm_sup:start_child(Vm).

start_link(Vm) ->
  gen_server:start_link(?MODULE, [Vm], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Vm]) ->
  ev8:set_vm_server(Vm, self()),
  {ok, #state{
      vm = Vm}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({call, Context, Fun, Args}, State) ->
  io:format("Calle: ~p~n", [Args]),
  {ok, Pid} = ev8call_srv:create(Context),
  ev8call_srv:call(Pid, Fun, Args),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("EXIT WITH REASON: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
