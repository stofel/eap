-module(eap_pool_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
  checkout/2,
  checkin/2,
  get_worker/1,
  start_worker/2,
  stat/1]).


-include("../include/eap.hrl").

start_link(PoolName) ->
  gen_server:start_link({local, PoolName}, ?MODULE, [], []).


%
init([]) ->
  process_flag(trap_exit, true),
  State = #{
      idle => [],  %% Idle worker pids for checkout
      rest => []   %% Chekined worker pids
    },
  {ok, State}.

%
terminate(_Reason, #{pool_name := PoolName}) -> 
  ?INF("terminate", {PoolName, self()}),
  ok.

%
code_change(_OldVersion, State, _Extra) -> 
{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Server api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% infos
handle_info({'EXIT', Pid}, State)           -> somebody_exit_(State, Pid);
handle_info({'DOWN',_R,process,Pid,_Res},S) -> somebody_exit_(S, Pid);
handle_info(Msg, S)                         -> ?INF("Unk msg:", Msg), {noreply, S}.
%% casts                          
handle_cast({checkout, Pid}, S)    -> checkout_(S, Pid);
handle_cast({checkin,  Pid}, S)    -> checkin_(S, Pid);
handle_cast({start_worker,Pid}, S) -> start_worker_(S, Pid);
handle_cast(_Req, S)               -> ?INF("Unknown cast", _Req), {noreply, S}.
%% calls                           
handle_call(get_worker, _From, S)  -> get_worker_(S);
handle_call(stat, _From, S)        -> stat_(S);
handle_call(_Req, _From, S)        -> {reply, ?e(unknown_command), S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
checkout(PoolName, Pid)             -> gen_server:cast(PoolName, {checkout, Pid}).
checkout_(S = #{idle := Pids}, Pid) -> {noreply, S#{idle := lists:delete(Pid, Pids)}}.


%%
checkin(PoolName, Pid)            -> gen_server:cast(PoolName, {checkin, Pid}).
checkin_(S = #{rest := R}, Pid)   -> 
  case lists:member(Pid, R) of
    false -> {noreply, S#{rest := [Pid|R]}};
    true  -> {noreply, S}
  end.


%%
start_worker(PoolName, Pid)           -> gen_server:cast(PoolName, {start_worker, Pid}).
start_worker_(S = #{idle := I}, Pid)  -> erlang:monitor(process, Pid), {noreply, S#{idle := lists:usort([Pid|I])}}.


%%
get_worker(PoolName)                               -> gen_server:call(PoolName, get_worker).
get_worker_(S = #{idle := [Pid|_Pids]})            -> {reply, {ok, Pid}, S};
get_worker_(S = #{idle := [], rest := [Pid|Pids]}) -> {reply, {ok, Pid}, S#{idle := [Pid|Pids], rest := []}};
get_worker_(S)                                     -> {reply, ?e(no_idle_worker_exists), S}.


%%
stat(PoolName)                    -> gen_server:call(PoolName, stat).
stat_(S = #{idle := W1, rest := W2})     -> {reply, #{workers_num => {length(W1), length(W2)}}, S}.


%%
somebody_exit_(S = #{idle := I}, Pid) ->
  ?INF("AAAAA", Pid),
  {noreply, S#{idle := lists:delete(Pid, I)}}.


