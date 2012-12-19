-module(v8call_srv).

-behaviour(gen_server).

%% API
-export([start_link/1,
        create/1,
        call/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    context}).

%%%===================================================================
%%% API
%%%===================================================================

call(Pid, Fun, Args) ->
  gen_server:cast(Pid, {call, Fun, Args}).

start_link(Context) ->
  gen_server:start_link(?MODULE, [Context], []).

create(Context) ->
  v8call_sup:start_child(Context).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Context]) ->
  {ok, #state{
      context = Context}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({call, Fun, Args}, State) ->
  Context = State#state.context,
  ev8:call_respond(Context, Fun, Args),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate({{badarity, _}, _}, State) ->
  Context = State#state.context,
  v8nif:execute(Context, self(), {call_respond, {error, badarity}}),
  ok;
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
