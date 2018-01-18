-module(dugong_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { dugong_vnode_master,
                  {riak_core_vnode_master, start_link, [dugong_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    CoverageFSMs = {dugong_coverage_fsm_sup,
                    {dugong_coverage_fsm_sup, start_link, []},
                    permanent, infinity, supervisor, [dugong_coverage_fsm_sup]},

    WriteFSMs = {dugong_write_fsm_sup,
                 {dugong_write_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [dugong_write_fsm_sup]},

    {ok, {{one_for_one, 5, 10}, [VMaster, CoverageFSMs, WriteFSMs]}}.
