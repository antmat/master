-module(master).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/0,start/2, stop/1,init/1]).

-export([ who_am_i/0,
		who_is_master/0,
		who_is_slaves/0,
		who_is_live/0,
		who_is_dead/0,
		cluster_status/0,
		connect/1
	]).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

start() -> application:start(master).

start(_,_) ->
	Min = [X || {minimal,X} <- application:get_all_env(master)],
	Path = [X || {path, X} <- application:get_all_env(master)],
	Slave_script = [X || {slave_script, X} <- application:get_all_env(master)],
	Master_script = [X || {master_script, X} <- application:get_all_env(master)],
	Dead_script = [X || {dead_script, X} <- application:get_all_env(master)],
	if 
		Min == [] -> io:format("You must set minimal in sys.config"),
			{error,bad_config};
		Path == [] -> io:format("You must set path in sys.config"),
			{error,bad_config};
		Slave_script == [] -> io:format("You must set slave_script in sys.config"),
			{error,bad_config};
		Master_script == [] -> io:format("You must set master_script in sys.config"),
			{error,bad_config};
		Dead_script == [] -> io:format("You must set dead_script in sys.config"),
			{error,bad_config};
		true ->	
			application:start(os_mon),
			supervisor:start_link({local,?MODULE},?MODULE, [])
	end.

stop(_) ->
        ok.

init(_) ->
	Server = {master_srv, {master_srv, start_link, []}, permanent, 500, worker, [master_srv]},
	Reconnect = {reconnect_srv, {reconnect_srv, start_link, []}, permanent, 500, worker, [reconnect_srv]},
	NewMaster = {master_local, {master_local, start_link, []}, permanent, 500, worker, [master_local]},
	{ok, {{one_for_one, 15, 5}, [Server, Reconnect, NewMaster]}}.

%% public API
who_am_i() -> 
	{Status, _Master} = master_srv:status(),
	Status.
who_is_master() -> 
	case master_srv:who_is_master() of
		[] -> none;
		[Master] -> Master
	end.
who_is_slaves() ->
	master_srv:who_is_slave().

who_is_live() -> [node()|nodes()].
who_is_dead() -> master_srv:who_is_dead().

cluster_status() ->
	case master_srv:who_is_master() of
		[] -> dead;
		[_A] -> work;
		_ -> dead
	end.

connect(Node) -> net_kernel:connect(Node).
