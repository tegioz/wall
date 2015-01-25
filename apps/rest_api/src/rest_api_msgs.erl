-module(rest_api_msgs).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         finish_request/2,
         malformed_request/2,
         post_is_create/2,
         process_post/2,
         to_json/2,
         service_available/2,
         resource_exists/2,
         valid_entity_length/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {db, data, body}).

init([]) ->
    {ok, #ctx{}}.
    %{{trace, "/tmp"}, #ctx{}}.

allowed_methods(ReqData, Ctx) ->
    {['GET','POST'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    db_api_sup_server:terminate_child(Ctx#ctx.db),
    {true, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
    case wrq:method(ReqData) of
        'POST' ->
            JSONChanges = wrq:req_body(ReqData),
            {Changes} = jiffy:decode(JSONChanges),
            Changes2 = [{binary_to_atom(Key, utf8), Value} || {Key, Value} <- Changes],
            UsedKeys = [Key || {Key, _Value} <- Changes2],
            UsedKeysSet = sets:from_list(UsedKeys),
            ValidKeysSet = sets:from_list([message]),
            case sets:is_subset(UsedKeysSet, ValidKeysSet) of
                true  -> Ctx2 = Ctx#ctx{body=Changes2},
                         {false, ReqData, Ctx2};
                false -> {true, ReqData, Ctx}
            end;
        _ ->
            {false, ReqData, Ctx}
    end. 

post_is_create(ReqData, Ctx) ->
    {false, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    NewDoc = bson:document(Ctx#ctx.body),
    ID = ets:update_counter(wall, msgs, 1),
    Agent = list_to_binary(wrq:get_req_header('User-Agent', ReqData)),
    IP = list_to_binary(wrq:peer(ReqData)),
    NewDoc2 = bson:append({'id', ID, 'agent', Agent, 'ip', IP}, NewDoc),
    case db_api_server:save(Ctx#ctx.db, NewDoc2) of
        {ok, done}       -> Body = jiffy:encode({[{'id', ID}, {'agent', Agent}, {'ip', IP}]}),
                            ReqData2 = wrq:set_resp_body(Body, ReqData),
                            {true, ReqData2, Ctx};
        {error, _Reason} -> {false, ReqData, Ctx}
    end.

to_json(ReqData, Ctx) ->
    Docs = lists:map(fun(Doc) -> 
        Doc2 = bson:exclude(['_id',ip],Doc),
        {bson:fields(Doc2)} 
    end, Ctx#ctx.data),
    JSONDocs = jiffy:encode(Docs),
    {JSONDocs, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    case db_api_server:all(Ctx#ctx.db) of
        {ok, Docs}       -> Ctx2 = Ctx#ctx{data=Docs},
                            {true, ReqData, Ctx2};
        {error, _Reason} -> {false, ReqData, Ctx}
    end.

service_available(ReqData, Ctx) ->
    case db_api_sup_server:start_child() of
        {ok, PID}        -> Ctx2 = Ctx#ctx{db=PID},
                            {true, ReqData, Ctx2};
        {error, _Reason} -> {false, ReqData, Ctx}
    end.

valid_entity_length(ReqData, Ctx) ->
    case length(binary:bin_to_list(wrq:req_body(ReqData))) > 200 of
        true  -> {false, ReqData, Ctx};
        false -> {true, ReqData, Ctx}
    end.
