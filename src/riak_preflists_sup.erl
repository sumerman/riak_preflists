-module(riak_preflists_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I),
        {I, {I, start_link, []}, 
         permanent, 5000, worker, [I]}).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
    {ok, { {one_for_one, 5, 10}, 
           [
            ?CHILD(riak_preflists_updates_handler)
           ]} }.

