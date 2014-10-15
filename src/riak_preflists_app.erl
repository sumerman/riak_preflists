%%%-------------------------------------------------------------------
%%% @author Valery Meleshkin 
%%% @copyright 2014 
%%%-------------------------------------------------------------------

-module(riak_preflists_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start/0]).

%% Application callbacks

start() ->
    application:ensure_all_started(riak_preflists, transient).

start(_StartType, _StartArgs) ->
    riak_core:wait_for_service(riak_kv),
    case riak_preflists_sup:start_link() of
        {error, _} = Err -> Err;
        {ok, _Pid} = OK ->
            [webmachine_router:add_route(R) 
             || R <- [{["riak_preflists", "notifications"], 
                       riak_preflist_notif_wm_entry, []},
                      {["riak_preflists", "nodelists", "buckets", bucket, "keys", key],
                       riak_preflist_nodelist_wm_entry, []},
                      {["riak_preflists", "ringstatus"],
                       riak_preflist_ringstatus, []}
                     ]],
            OK
    end.

stop(_State) ->
    ok.
