
-module(rest_api_sup).
-behaviour(supervisor).
-export([start_link/0, upgrade/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

init([]) ->
    ets:new(wall, [set, public, named_table]),
    ets:insert(wall,{msgs,0}),
    
    Ip = case application:get_env(rest_api, ip) of
        {ok, IpConf} -> IpConf;
        undefined    -> "0.0.0.0"
    end,
    Port = case application:get_env(rest_api, port) of
        {ok, PortConf} -> PortConf;
        undefined      -> 8000 
    end,
    
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
