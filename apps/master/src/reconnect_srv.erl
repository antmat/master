-module(reconnect_srv).

%% API
-export([start_link/0, reconnect/1]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {bad_count = 0 }).

reconnect(Node) -> gen_server:cast(?SERVER,{reconnect,Node}).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_cast({reconnect,Node}, State) ->
	timer:send_after(2000,{reconnect, Node}),
	{noreply, State};

handle_cast(_Msg, State) ->
        {noreply, State}.

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

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.



