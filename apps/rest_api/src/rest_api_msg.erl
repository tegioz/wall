-module(rest_api_msg).

-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_completed/2,
         delete_resource/2,
         finish_request/2,
         malformed_request/2,
         from_json/2,
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
    {['DELETE','GET','PUT'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

delete_completed(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

delete_resource(ReqData, Ctx) ->
     ID = bson:at('_id', Ctx#ctx.data),
     case db_api_server:delete(Ctx#ctx.db, ID) of
        {ok, done}       -> {true, ReqData, Ctx};
        {error, _Reason} -> {false, ReqData, Ctx}
     end.

finish_request(ReqData, Ctx) ->
    db_api_sup_server:terminate_child(Ctx#ctx.db),
    {true, ReqData, Ctx}.

from_json(ReqData, Ctx) ->
    Doc = Ctx#ctx.data,
    Body = Ctx#ctx.body,
    ID = bson:at('_id', Doc),
    NewDoc = bson:document(Body),
    NewDoc2 = bson:append({'_id', ID}, NewDoc),
    NewDoc3 = bson:merge(NewDoc2, Doc),
    case db_api_server:save(Ctx#ctx.db, NewDoc3) of
        {ok, done}       -> {true, ReqData, Ctx};
        {error, _Reason} -> {false, ReqData, Ctx}
    end.

malformed_request(ReqData, Ctx) ->
    case wrq:method(ReqData) of
        'PUT' ->
            JSONChanges = wrq:req_body(ReqData),
            {ok, {Changes}} = json:decode(JSONChanges),
            Changes2 = [{binary_to_atom(Key, utf8), Value} || {Key, Value} <- Changes],
            UsedKeys = [Key || {Key, _Value} <- Changes2],
            UsedKeysSet = sets:from_list(UsedKeys),
            ValidKeysSet = sets:from_list([agent,id,ip,message]),
            case sets:is_subset(UsedKeysSet, ValidKeysSet) of
                true  -> Ctx2 = Ctx#ctx{body=Changes2},
                         {false, ReqData, Ctx2};
                false -> {true, ReqData, Ctx}
            end;
        _ ->
            {false, ReqData, Ctx}
    end.  

to_json(ReqData, Ctx) ->
    Doc = Ctx#ctx.data, 
    Doc2 = bson:exclude(['_id'], Doc),
    {ok, JSONDoc} = json:encode({bson:fields(Doc2)}),
    {JSONDoc, ReqData, Ctx}.

resource_exists(ReqData, Ctx) ->
    ID = wrq:path_info(id, ReqData),
    case db_api_server:find(Ctx#ctx.db, ID) of
        {ok, notFound}   -> {false, ReqData, Ctx};
        {ok, Doc}        -> Ctx2 = Ctx#ctx{data=Doc},
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
    case length(binary:bin_to_list(wrq:req_body(ReqData))) > 500 of
        true  -> {false, ReqData, Ctx};
        false -> {true, ReqData, Ctx}
    end.
