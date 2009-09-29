-module(director).
-export([start/0, stop/0]).

start() ->
    application:start(inets),
    application:start(crypto),
    application:start(director).

stop() ->
    ok.


