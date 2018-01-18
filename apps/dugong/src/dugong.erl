-module(dugong).

-export([
         ping/0,
         set_device_value/3,
         get_device_value/2,
         keys/1
        ]).

-define(DEVICE_PREFIX, {<<"dugong">>, <<"config">>}).

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

set_device_value(DeviceRef, Pin, Value) ->
    {Host, _Port} = riak_core_metadata:get(?DEVICE_PREFIX, DeviceRef),

    dugong_comm_udp_server:device_client(Host, << 0, Pin, Value >> ),

    ReqID = make_ref(),
    Timeout = 5000,
    dugong_write_fsm:write(?N, ?W, {DeviceRef, Pin}, Value, self(), ReqID),
    wait_for_reqid(ReqID, Timeout).

get_device_value(DeviceRef, Pin) ->
    send_to_one(DeviceRef, {get, {DeviceRef, Pin}}).

keys(Bucket) ->
    Timeout = 5000,
    dugong_coverage_fsm:start({keys, Bucket}, Timeout).

send_to_one(Key, Cmd) ->
    DocIdx = riak_core_util:chash_key(Key),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, dugong),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, dugong_vnode_master).

wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, {error, Reason}} -> {error, Reason};
        {ReqID, Val}             -> Val
    after Timeout                -> {error, timeout}
    end.
