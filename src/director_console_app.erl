-module(director_console_app).

-behaviour(application).
-export([start/2,stop/1]).


start(_Type, _StartArgs) ->
    director_console_sup:start_link().

stop(_State) ->
    ok.
