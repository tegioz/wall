
-module(rest_api_app).
-behaviour(application).
-export([start/2,stop/1]).


start(_Type, _StartArgs) ->
    rest_api_sup:start_link().

stop(_State) ->
    ok.
