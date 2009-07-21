-module(director_console).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
start() ->
    ensure_started(crypto),
    application:start(director_console).

stop() ->
    Res = application:stop(director_console),
    application:stop(crypto),
    Res.
