%%%-------------------------------------------------------------------
%% @doc eap top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eap_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(PoolName) ->
    SupName = list_to_atom(atom_to_list(PoolName) ++ "pool_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, PoolName).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(PoolName) ->
    SupFlags = #{
      strategy => one_for_all,
      intensity => 100, %% I think it should never shutdown
      period => 1},

    WorkersSup = #{
      id        => workers_sup,
      start     => {eap_workers_sup, start_link, [PoolName]},
      restart   => permanent,
      shutdown  => 10000,
      type      => supervisor},

    PoolServer = #{
      id        => pool_server,
      start     => {eap_pool_server, start_link, [PoolName]},
      restart   => permanent,
      shutdown  => 3000,
      type      => worker},
    {ok, {SupFlags, [WorkersSup, PoolServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
