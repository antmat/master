-module(master).

-export([start/0]).

start() -> 
	application:start(master).
