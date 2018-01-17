-module(dugong_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case dugong_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, dugong_vnode}]),
            ok = riak_core_node_watcher:service_up(dugong, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
