-module(master_leader).

-behaviour(gen_leader).

%% API
-export([start_link/0,start_link/1, start_link/2, status/0]).

%% gen_leader callbacks
-export([init/1,
         handle_cast/3,
         handle_call/4,
         handle_info/2,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/3,
         surrendered/3,
         from_leader/3,
         code_change/4,
         terminate/2]).


-define(SERVER, ?MODULE).

-record(state, {state}).


status() ->
	gen_leader:call(?MODULE,status).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	[Live] = [X || {live,X} <- application:get_all_env(master)],
	Connect = lists:map(
		fun(X) ->
				net_kernel:connect(X)
		end,Live),
	Start = lists:foldl(fun(X,Y) -> X or Y end,false,Connect),
	case Start of
		true -> start_link([node()|nodes()]);
		false -> 
			error("cant connect to nodes ~p ~n",[Live])
	end.

start_link(Nodes) ->
    start_link(Nodes, []).

start_link(Nodes, Seed) when is_list(Nodes), is_atom(Seed) ->
    start_link(Nodes, {seed_node, Seed});

start_link(Nodes, Opts) ->
    gen_leader:start_link(?SERVER, Nodes, Opts, ?MODULE, [], []).

%%%===================================================================
%%% gen_leader callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
%%
%% @spec elected(State, Election, undefined) -> {ok, Synch, State}
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, undefined) ->
	case nodes() of
		[] -> io:format("is slave \n "),
			[SlaveScript] = [X || {slave_script,X} <- application:get_all_env(master)],
			spawn(fun() -> os:cmd(SlaveScript) end),
		    {ok, [], State#state{state = slave}};
		_ ->
			[MasterScript] = [X || {master_script,X} <- application:get_all_env(master)],
			io:format("is master \n"),
			spawn(fun() -> os:cmd(MasterScript) end),
			{ok, [], State#state{state = master}}
	end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
%%
%% @spec elected(State, Election, Node) -> {ok, Synch, State}
%% @end
%%--------------------------------------------------------------------
elected(State, _Election, _Node) ->
	io:format("node was added  ~p \n",[_Node]),
	case State#state.state of
		master -> {reply, [], State};
		slave ->
        	[MasterScript] = [X || {master_script,X} <- application:get_all_env(master)],
	        io:format("is master \n"),
	        spawn(fun() -> os:cmd(MasterScript) end),
		   	{reply, [], State#state{state = master}}
	end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
%%
%% @spec surrendered(State, Synch, Election) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
surrendered(State, _Synch, _Eelection) ->
	io:format("is slave \n "),
	[SlaveScript] = [X || {slave_script,X} <- application:get_all_env(master)],
	spawn(fun() -> os:cmd(SlaveScript) end),
    {ok, State#state{state = slave}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. Called in the leader.
%%
%% @spec handle_leader_call(Request, From, State, Election) ->
%%                                            {reply, Reply, Broadcast, State} |
%%                                            {reply, Reply, State} |
%%                                            {noreply, State} |
%%                                            {stop, Reason, Reply, State} |
%%                                            {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_leader_call(_Request, _From, State, _Election) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages. Called in the leader.
%%
%% @spec handle_leader_cast(Request, State, Election) ->
%%                                            {ok, Broadcast, State} |
%%                                            {noreply, State} |
%%                                            {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling messages from leader.
%%
%% @spec from_leader(Request, State, Election) ->
%%                                    {ok, State} |
%%                                    {noreply, State} |
%%                                    {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
from_leader(_Synch, State, _Election) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling nodes going down. Called in the leader only.
%%
%% @spec handle_DOWN(Node, State, Election) ->
%%                                  {ok, State} |
%%                                  {ok, Broadcast, State} |
%% @end
%%--------------------------------------------------------------------
handle_DOWN(_Node, State = {state,master}, _Election) ->
	case nodes() of
		[] -> io:format("is slave \n "),
			[SlaveScript] = [X || {slave_script,X} <- application:get_all_env(master)],
			spawn(fun() -> os:cmd(SlaveScript) end),
			timer:send_after(1000,recheck),
			{ok, State#state{state = slave}};
		_ -> {ok, State}
	end;

handle_DOWN(_Node,State,_Election) ->
	{ok,State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State, Election) ->
%%                                   {reply, Reply, State} |
%%                                   {noreply, State} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(status, _From, State, _Election) ->
	{reply, State#state.state, State};

handle_call(_Request, _From, State, _Election) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State, Election) ->
%%                                  {noreply, State} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(recheck,State) ->
	case length(nodes()) of
		0 -> timer:send_after(1000,recheck),
			{noreply,State};
		_ -> {noreply,State}
	end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_leader when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_leader terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Election, Extra) ->
%%                                          {ok, NewState} |
%%                                          {ok, NewState, NewElection}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

