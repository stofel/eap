%%%-------------------------------------------------------------------
%% @doc eap top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eap_workers_sup).

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
    SupName = list_to_atom(atom_to_list(PoolName) ++ "pool_workers_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
      strategy  => one_for_one,
      intensity => 10000, %% I think it should never shutdown
      period    => 1},
    {ok, {SupFlags, []}}.

