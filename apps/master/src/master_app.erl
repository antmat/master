-module(master_app).

-behaviour(application).

%% Application callbacks
-export([start/0,start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> start(a,a).

start(_StartType, _StartArgs) ->
    master_sup:start_link().

stop(_State) ->
    ok.
