-module(master).

-export([start/0,stop/0]).

start() -> 
	application:start(master).

stop() ->
	application:stop(master).
