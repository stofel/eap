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
  stat/1]).


-include("../include/eap.hrl").

start_link(PoolName) ->
  gen_server:start_link({local, PoolName}, ?MODULE, [], []).


%
init([]) ->
  process_flag(trap_exit, true),
  State = #{
      idle => []  %% Idle worker pids
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
handle_info({'EXIT', Pid}, State)  -> somebody_exit_(State, Pid);
handle_info(Msg, S)                -> ?INF("Unk msg:", Msg), {noreply, S}.
%% casts                          
handle_cast({checkout, Pid}, S)    -> checkout_(S, Pid);
handle_cast({checkin,  Pid}, S)    -> checkin_(S, Pid);
handle_cast(_Req, S)               -> ?INF("Unknown cast", _Req), {noreply, S}.
%% calls                           
handle_call(get_worker, _From, S)  -> get_worker_(S);
handle_call(stat, _From, S)        -> stat_(S);
handle_call(_Req, _From, S)        -> {reply, ?e(unknown_command), S}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
checkout(PoolName, Pid)          -> gen_server:cast(PoolName, {checkout, Pid}).
checkout_(S = #{idle := I}, Pid) -> {noreply, S#{idle := lists:delete(Pid, I)}}.



%%
checkin(PoolName, Pid)          -> gen_server:cast(PoolName, {checkin, Pid}).
checkin_(S = #{idle := I}, Pid) -> {noreply, S#{idle := lists:usort([Pid|I])}}.


%%
get_worker(PoolName)                 -> gen_server:call(PoolName, get_worker).
get_worker_(S = #{idle := [W|Rest]}) -> {reply, {ok, W}, S#{idle := Rest}};
get_worker_(S = #{idle := []})       -> {reply, ?e(no_idle_worker_exists), S}.


%%
stat(PoolName)                -> gen_server:call(PoolName, get_stat).
stat_(S = #{idle := Workers}) -> {reply, #{idle_workers_num => length(Workers)}, S}.


%%
somebody_exit_(S = #{idle := I}, Pid) ->
  {noreply, S#{idle := lists:delete(Pid, I)}}.

