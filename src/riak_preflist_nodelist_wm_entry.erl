%%%-------------------------------------------------------------------
%%% @author Valery Meleshkin 
%%% @copyright 2014 Exante
%%%-------------------------------------------------------------------

-module(riak_preflist_nodelist_wm_entry).

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
    Ctx1 = orddict:fold(
             fun
                 (bucket, B, C) ->
                     C#ctx{ bucket = iolist_to_binary(B) };
                 (key, K, C) ->
                     C#ctx{ key = iolist_to_binary(K) };
                 (_, _, C) ->
                     C
             end, Ctx, wrq:path_info(ReqData)),
    case Ctx1 of
        #ctx{ bucket = undefined } ->
            {true, ReqData, Ctx};
        #ctx{ key = undefined } ->
            {true, ReqData, Ctx};
        #ctx{} ->
            {false, ReqData, Ctx1}
    end.

service_available(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

to_json(ReqData, #ctx{ bucket = B,
                       key = K } = Ctx) ->
    Nodelist = lists:map(fun host_from_nodename/1,
                         riak_preflists_updates_handler:nodelist(B, K)),
    Body = mochijson2:encode(Nodelist),
    {[Body, $\n], ReqData, Ctx}.

host_from_nodename(Node)
  when is_atom(Node)->
    [_, Host] = binary:split(erlang:atom_to_binary(Node, latin1), <<"@">>),
    Host.

