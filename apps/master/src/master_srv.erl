-module(master_srv).

%% public API
-export([start_link/0,
		status/0, 
		who_is_master/0, 
		who_is_slave/0, 
		who_is_dead/0, 
		set_master/1, 
		slave_work/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {nodes=[], 
		status = slave, 
		active=[], 
		dead=[], 
		master, 
		minimal = 2, 
		path = "./nodes", 
		master_script,
		slave_script,
		dead_script,
		bad_count = 0
	}).

% Public API
who_is_master() -> 
	Nodes = [node()|nodes()],
	Z = lists:map(fun(X) -> gen_server:call({global,X},who_is_who) end, Nodes),
	[ X || {X, master} <- Z].

who_is_slave() ->
	Nodes = [node()|nodes()],
	Z = lists:map(fun(X) -> gen_server:call({global,X},who_is_who) end, Nodes),
	[ X || {X, slave} <- Z].
who_is_dead() ->
	gen_server:call({global,node()}, who_is_dead).

set_master(Node) ->
	lists:map(fun(X) -> gen_server:call({global,X}, {set_master,Node}) end, [node()|nodes()]).

slave_work(Nodes) ->
	lists:map(fun(X) -> gen_server:call({global,X}, slave_work) end, Nodes). 

status() -> 
	gen_server:call({global,node()},status).

start_link() ->
	Node = node(),
	gen_server:start_link({global,Node},?MODULE, #state{}, []).

% gen_server callbacks
init(State) ->
	net_kernel:monitor_nodes(true),
	net_kernel:set_net_ticktime(2),
	[Min] = [X || {minimal,X} <- application:get_all_env(master)],
	[Path] = [X || {path, X} <- application:get_all_env(master)],
	[Slave_script] = [X || {slave_script, X} <- application:get_all_env(master)],
	[Master_script] = [X || {master_script, X} <- application:get_all_env(master)],
	[Dead_script] = [X || {dead_script, X} <- application:get_all_env(master)],
	Nodes = load(Path),
	lists:map(fun(X) -> net_kernel:connect(X) end, Nodes),
	Active = [node()|nodes()],
	Dead = Nodes -- Active,
	%make_work(skip),
	{ok, State#state{ nodes = lists:usort([node()|Nodes])
			   , active = Active
			   , status = slave
			   , master = none
			   , path = Path
			   , minimal = Min
			   , master_script = Master_script
			   , slave_script = Slave_script
			   , dead_script = Dead_script
		       , dead = Dead }}.

% deactivate cluster
handle_call({set_master,none}, _, State = #state{master = Master, dead_script = Dead_script}) ->
	case Master of
		none -> skip;
		_ ->  make_work(skip,Dead_script)
	end,
	{reply, ok, State#state{status = slave, master = none}};

% callback for execute master slave scripts
handle_call({set_master,message}, _F, State = #state{status = master, master_script = Master_script}) ->
	make_work(master, Master_script),
	slave_work(nodes()),
	{reply, ok, State};

% callback for change status
handle_call({set_master,Master}, _, State = #state{master_script = Master_script}) ->
    case Master =:= node() of
		true -> 
			case State#state.status of
				master -> skip;
				_ -> 
					make_work(master, Master_script),
					slave_work(nodes())
			end,
		   	{reply, ok, State#state{status = master, master = Master}};
		false -> {reply, ok, State#state{status = slave, master = Master}}
	end;

% callback for execute slave script
handle_call(slave_work, _, State = #state{status = slave, slave_script = Slave_script}) ->
	make_work(slave, Slave_script),
	{reply, ok, State};

handle_call(who_is_who, _, State = #state{status = Status}) ->
	{reply,{node(),Status},State};

handle_call(who_is_dead,_, State = #state{dead = Dead}) ->
	{reply, Dead, State};

handle_call(status, _, State = #state{status = Status, master = Master}) ->
	{reply, {Status,Master}, State};

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

% try reconnect to node
handle_info({reconnect, Node},State = #state{ bad_count = Bad}) ->
	case net_kernel:connect(Node) of
		true -> {noreply,State#state{bad_count = 0}};
		false ->
			if
				( Bad =< 10 ) -> timer:send_after(2000, {reconnect, Node}),
					{noreply, State#state{bad_count = Bad +1}};
				( Bad > 10 ) -> timer:send_after(4000, {reconnect, Node}),
					{noreply, State#state{bad_count = Bad + 1}};
				( Bad > 20 ) -> timer:send_after(8000, {reconnect, Node}),
					{noreply, State#state{bad_count = Bad +1}};
				( Bad > 30 ) -> timer:send_after(16000, {reconnect, Node}),
					{noreply, State#state{bad_count = Bad +1}};
				( Bad > 40 ) -> timer:send_after(32000, {reconnect, Node}),
					{noreply, State#state{bad_count = Bad +1}}
			end
	end;

% node up message
handle_info({nodeup,Node},State = #state{ nodes = Nodes
		                                  , active = Active
										  , minimal = Min
										  , path = Path
										  , dead = Dead}) ->
	case lists:member(Node,Nodes) of
		true -> skip;
		false -> save([Node|Nodes],Path)
	end,
	spawn(fun() -> make_new_master(lists:usort([Node|Active]),Min) end),
	{noreply,State#state{ nodes = lists:usort([Node|Nodes])
			            , active = lists:usort([Node|Active])
						, dead = Dead -- [Node] }};

% node down messate
handle_info({nodedown,Node},State = #state{ minimal = Min
		                                    , active = Active
											, dead = Dead}) ->
	timer:send_after(2000,{reconnect, Node}),
    spawn(fun() -> make_new_master(lists:usort(Active -- [Node]), Min) end),
	{noreply,State#state{ active = Active -- [Node]
			            , dead = lists:usort([Node|Dead])}};

handle_info(_Info, State) ->
	io:format("info ~p~n",[_Info]),
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.


%% Internal functions
load(Path) -> 
	case file:consult(Path) of
        {ok,[Data]} -> Data;
        _ -> []
	end.

save(Nodes,Path) -> file:write_file(Path,io_lib:fwrite("~p.\n",[Nodes])).

make_new_master(Active,Min) ->
	timer:sleep(300),
	case length(Active) >= Min of
		false -> set_master(none);
		true -> make_new_master(Active)
	end.
make_new_master(Active) ->
	case who_is_master() of
		[] ->
			NextMaster = minimal_cpu(Active),
			if
			   	NextMaster =:= node() -> set_master(NextMaster);
				true -> skip
			end;
		[M] -> 
			if
				M =:= node() -> set_master(message);
				true -> skip
			end
	end.

minimal_cpu(Nodes) -> 
	All = [{rpc:call(X,cpu_sup,avg5,[]),X} || X <- Nodes ],
	minimal_cpu(All,hd(All)).

minimal_cpu([],{_,Win}) -> Win;
minimal_cpu([{Cpu,Node_name}|Tail],Win = {Cpu_win,_}) ->
	if
		Cpu < Cpu_win -> minimal_cpu(Tail,{Cpu,Node_name});
		Cpu > Cpu_win -> minimal_cpu(Tail,Win);
		Cpu == Cpu_win -> minimal_cpu(Tail,Win)
	end.


make_work(slave, Path) -> 
	io:format("i am slave"),
	spawn(fun() -> os:cmd(Path) end);
make_work(master, Path) ->
   io:format("i am master"),
	spawn(fun() -> os:cmd(Path) end);
make_work(skip, Path) -> 
	io:format("cluster is not active"),
	spawn(fun() -> os:cmd(Path) end);
make_work(_, _) -> error.
