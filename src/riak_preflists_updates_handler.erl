%%%-------------------------------------------------------------------
%%% @author Valery Meleshkin 
%%% @copyright 2014 
%%%-------------------------------------------------------------------

-module(riak_preflists_updates_handler).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0, subscribe/0, nodelist/2]).

%% Internal API
-export([membership_updated/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% API Function Definitions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec subscribe() -> ok | already_subscribed | {error, term()}.
subscribe() ->
    gen_server:call(?SERVER, {subscribe, self()}).

-type nodelist() :: [node()].

-spec nodelist(binary(), binary()) -> nodelist().
nodelist(Bucket, Key) ->
    BucketProps = riak_core_bucket:get_bucket(Bucket),
    N = proplists:get_value(n_val, BucketProps),
    Idx = riak_core_util:chash_key({Bucket, Key}),
    Preflist = riak_core_apl:get_apl(Idx, N, riak_kv),
    refine_plist(Preflist, ordsets:new()).

%% gen_server Function Definitions

-record(state, { subs=ordsets:new() }).

init(_Args) ->
    subscribe_to_ring_changes(),
    {ok, #state{}}.

handle_call({subscribe, Pid}, _From, 
            #state{subs=Subs} = State) ->
    Reply = case ordsets:is_element(Pid, Subs) of
                true  -> already_subscribed;
                false ->
                    erlang:monitor(process, Pid),
                    ok
            end,
    {reply, Reply,
     State#state{subs=ordsets:add_element(Pid,Subs)}}.

handle_cast(membership_updated, State) ->
    [S ! membership_updated || S <- State#state.subs],
    {noreply, State}.

handle_info({'DOWN', _MonRef, _T, Pid, _Inf},
            #state{subs=Subs} = State) ->
    {noreply, 
     State#state{subs=ordsets:del_element(Pid, Subs)}};

handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    {stop, {handler_down, Handler, Reason}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

subscribe_to_ring_changes() ->
    riak_core_ring_events:add_sup_callback(fun ?MODULE:membership_updated/1),
    riak_core_node_watcher_events:add_sup_callback(fun ?MODULE:membership_updated/1).

membership_updated(_Upd) ->
    {ok, Ring} = riak_core_ring_manager:get_raw_ring(),
    Single = ([node()] =:= riak_core_ring:all_members(Ring)),
    case {Single, riak_core_ring:pending_changes(Ring)} of
        {false, []} -> membership_updated();
        {_, _}      -> ok
    end.

membership_updated() ->
    catch gen_server:cast(?SERVER, membership_updated).

refine_plist([], Nodes) -> Nodes;
refine_plist([{_PartIdx, Node} | Plist], Nodes) ->
    refine_plist(Plist, ordsets:add_element(Node, Nodes)).

