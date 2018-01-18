-module(dugong).

-export([
         ping/0,
         write_pin_value/3
        ]).

-ignore_xref([
              ping/0
             ]).

-define(N, 3).
-define(W, 3).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, dugong),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, dugong_vnode_master).

write_pin_value(_Pin, _Value, _MetaData) ->
    ok.

put(Key, Value) ->
    % tanodb_metrics:core_put(),
    ReqID = make_ref(),
    Timeout = 5000,
    dugong_write_fsm:write(?N, ?W, Key, Value, self(), ReqID),
    wait_for_reqid(ReqID, Timeout).

wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, {error, Reason}} -> {error, Reason};
        {ReqID, Val}             -> Val
    after Timeout                -> {error, timeout}
    end.
