
-module(master_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	net_kernel:connect('master@antibot.nigma.ru'),
	Child = [?CHILD(master_srv,worker),
		{master_leader,{master_leader,start_link,[[node()|nodes()]]},permanent,5000,worker,[master_leader]}],
    {ok, { {one_for_one, 5, 10}, Child} }.

