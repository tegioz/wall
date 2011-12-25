
-module(db_api_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupervisorSpec = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,
    PoolSupervisor = {db_api_sup_pool, {db_api_sup_pool, start_link, []}, Restart, Shutdown, Type, [db_api_sup_pool]}, 
    
    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,
    ServerSupervisor = {db_api_sup_server, {db_api_sup_server, start_link, []}, Restart, Shutdown, Type, [db_api_sup_server]}, 

    {ok, {SupervisorSpec, [PoolSupervisor, ServerSupervisor]}}.

