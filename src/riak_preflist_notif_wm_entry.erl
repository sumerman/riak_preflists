%%%-------------------------------------------------------------------
%%% @author Valery Meleshkin 
%%% @copyright 2014 Exante
%%%-------------------------------------------------------------------

-module(riak_preflist_notif_wm_entry).

%% webmachine resource exports
-export([
         init/1,
         content_types_provided/2,
         service_available/2,
         malformed_request/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-define(DEFAULT_TIMEOUT, "1").

-record(ctx, { ping_interval }).

init(_) ->
    {ok, #ctx{}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}],
     ReqData, Context}.

service_available(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

malformed_request(ReqData, Ctx) ->
    TimeoutStr = wrq:get_qs_value("ping", ?DEFAULT_TIMEOUT, ReqData),
    try list_to_integer(TimeoutStr) of
        T when is_integer(T) ->
            {false, ReqData, Ctx#ctx{ ping_interval = T*1000 }}
    catch
        error:_ ->
            {true, ReqData, Ctx}
    end.

to_json(ReqData, #ctx{ ping_interval = T } = Ctx) ->
    StreamFun = stream_helper(T),
    riak_preflists_updates_handler:subscribe(),
    {{stream, {<<>>, StreamFun}}, ReqData, Ctx}.

stream_helper(Timeout) ->
    fun() ->
            receive
                membership_updated ->
                    {<<"update\n">>,
                     stream_helper(Timeout)}
            after Timeout ->
                    {<<"ping\n">>,
                     stream_helper(Timeout)}
            end
    end.

