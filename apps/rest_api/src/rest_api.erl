-module(rest_api).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(syntax_tools),
    ensure_started(compiler),
    ensure_started(xmerl),
    ensure_started(asn1),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, webmachine_logger),
    ensure_started(webmachine),
    ensure_started(mongodb),
    ensure_started(db_api),
    ensure_started(rest_api),
    rest_api_sup:start_link().

stop() ->
    ok.
