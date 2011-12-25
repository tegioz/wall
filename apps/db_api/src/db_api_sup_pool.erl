
-module(db_api_sup_pool).
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
    Ip = case application:get_env(db_api, server_ip) of
        {ok, ServerIp} -> ServerIp;
        undefined      -> "localhost"
    end,
    Port = case application:get_env(db_api, server_port) of
        {ok, ServerPort} -> ServerPort;
        undefined        -> 27017
    end,
    
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupervisorSpec = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,
    Host = {Ip, Port},
    ChildSpec = {db_api_pool, {db_api_pool, start_link, [Host]}, Restart, Shutdown, Type, [db_api_pool]}, 
    
    {ok, {SupervisorSpec, [ChildSpec]}}.

