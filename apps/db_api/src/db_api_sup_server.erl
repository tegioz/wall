
-module(db_api_sup_server).
-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/0,
         terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

terminate_child(PID) ->
    db_api_server:terminate(PID).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupervisorSpec = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,
    ChildSpec = {db_api_server, {db_api_server, start_link, []}, Restart, Shutdown, Type, [db_api_server]}, 
    
    {ok, {SupervisorSpec, [ChildSpec]}}.

