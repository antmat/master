-module(master_local).

-export([start_link/0, new_master/2]).

-export([init/1,
         handle_cast/2,
         terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {count}).

new_master(Active,Min) ->
	gen_server:cast({new,Active,Min}).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
        {ok, #state{}}.


handle_cast({new,Active,Min}, State) ->
	make_new_master(Active,Min),
	{noreply,State};

handle_cast(_Msg, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

% internal API
make_new_master(Active,Min) ->
	case length(Active) >= Min of
		false -> master_srv:set_master(none);
		true -> make_new_master(Active)
	end.
make_new_master(Active) ->
	case master_srv:who_is_master() of
		[] ->
			NextMaster = minimal_cpu(Active),
			if
			   	NextMaster =:= node() -> master_srv:set_master(NextMaster);
				true -> skip
			end;
		[M] -> 
			if
				M =:= node() -> master_srv:set_master(message);
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


