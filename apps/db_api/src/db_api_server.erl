
-module(db_api_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         all/1,
         delete/2,
         find/2,
         save/2,
         terminate/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Records
-record(state, {db_conn, db}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all(PID) ->
    gen_server:call(PID, all).

delete(PID, ID) ->
    gen_server:call(PID, {delete, ID}).

find(PID, ID) ->
    gen_server:call(PID, {find, ID}).

save(PID, NewDoc) ->
    gen_server:call(PID, {save, NewDoc}).

terminate(PID) ->
    gen_server:call(PID, terminate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Database = case application:get_env(db_api, database) of
        {ok, DatabaseConf} -> DatabaseConf;
        undefined          -> wall
    end,
    case db_api_pool:get() of
        {ok, Conn}      -> {ok, #state{db_conn=Conn,db=Database}};
        {error, Reason} -> {error, Reason}
    end.

handle_call(all, _From, State) ->
    case mongo:do(safe, master, State#state.db_conn, State#state.db, fun() ->
        %mongo:rest(mongo:find(items, {'$query', {}, '$orderby', {order,1}}, {'_id', 1, id, 1, message, 1}))
        mongo:rest(mongo:find(items, {'$query', {}, '$orderby', {id,1}}, {}, 0, 10))
    end) of
        {ok, Docs}        -> {reply, {ok, Docs}, State};
        {failure, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({delete, ID}, _From, State) ->
    case mongo:do(safe, master, State#state.db_conn, State#state.db, fun() ->
        mongo:delete(items, {'_id', ID}) 
    end) of
        {ok, ok}    -> {reply, {ok, done}, State};
        {_, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({find, ID}, _From, State) ->
    case mongo:do(safe, master, State#state.db_conn, State#state.db, fun() ->
        mongo:find_one(items, {id, list_to_integer(ID)}) 
    end) of
        {ok, {Doc}}       -> {reply, {ok, Doc}, State};
        {ok, {}}          -> {reply, {ok, notFound}, State};
        {failure, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({save, NewDoc}, _From, State) ->
    case mongo:do(safe, master, State#state.db_conn, State#state.db, fun() ->
        mongo:save(items, NewDoc) 
    end) of
        {ok, ok}          -> {reply, {ok, done}, State};
        {failure, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call(terminate, _From, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

