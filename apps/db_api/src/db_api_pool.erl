
-module(db_api_pool).
-behaviour(gen_server).

%% API
-export([start_link/1,
         get/0,
         close/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Records
-record(state, {pool}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Host, []).

get() ->
    gen_server:call(?MODULE, get).

close() ->
    gen_server:call(?MODULE, terminate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Host) ->
    Pool = resource_pool:new(mongo:connect_factory(Host), 1000),
    {ok, #state{pool=Pool}}.

handle_call(get, _From, State) ->
    {reply, resource_pool:get(State#state.pool), State};
handle_call(close, _From, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

