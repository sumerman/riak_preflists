%%%-------------------------------------------------------------------
%%% @author Damien Krotkine 
%%% @copyright 2014
%%%-------------------------------------------------------------------

-module(riak_preflist_ringstatus).

%% webmachine resource exports
-export([
         init/1,
         content_types_provided/2,
         service_available/2,
         malformed_request/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, { bucket = undefined, 
               key = undefined
             }).

init(_) ->
    {ok, #ctx{}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}],
     ReqData, Context}.

malformed_request(ReqData, Ctx) ->
    {false, ReqData, Ctx}.

service_available(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    { ok, Ring } = riak_core_ring_manager:get_my_ring(),
    Members = riak_core_ring:all_owners(Ring),
    Body = mochijson2:encode(Members),
    {[Body, $\n], ReqData, Ctx}.

