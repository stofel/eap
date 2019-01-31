-module(eap).

-export([start/1, stop/1]).

-export([cast/2]).


-include("../include/eap.hrl").


-spec start(Args::map()) -> ok.
start(_Args = #{pool_name := PoolName, mfa := {M,F,A}, size := Size}) ->
  PoolSpecs = #{
      id        => PoolName,
      start     => {eap_pool_sup, start_link, [PoolName]},
      restart   => permanent,
      shutdown  => 20000,
      type      => supervisor
      },
  case supervisor:start_child(eap_sup, PoolSpecs) of
    {ok, _} -> 
      %%start workers
      WorkersSupName = list_to_atom(atom_to_list(PoolName) ++ "pool_workers_sup"),
      StartWorkerFun = fun
        (Fu, N) when N > 0 ->
          WorkerSpecs = #{
            id        => N,
            start     => {M, F, A},
            restart   => permanent,
            shutdown  => 1000,
            type      => worker
          },
          case supervisor:start_child(WorkersSupName, WorkerSpecs) of
            {ok, _ } -> Fu(Fu, N-1);
            Else -> ?e(fail_pool_worker_start, Else)
          end;
        (_F, 0) -> ok
      end,
      case StartWorkerFun(StartWorkerFun, Size) of 
        ok -> ok;
        Else ->
          supervisor:terminate_child(eap_sup, PoolName),
          supervisor:delete_child(eap_sup, PoolName),
          Else
      end;
    Else -> ?e(fail_pool_sup_start, Else)
  end.
  

%
stop(PoolName) ->
  supervisor:terminate_child(eap_sup, PoolName),
  supervisor:delete_child(eap_sup, PoolName).

%
cast(Msg, PoolName) when is_atom(PoolName) ->
  case eap_pool_server:get_worker(PoolName) of
    {ok, Pid} -> gen_server:cast(Pid, Msg);
    Else -> Else
  end;
cast(_,_) -> ?e(wrong_pool_name).
