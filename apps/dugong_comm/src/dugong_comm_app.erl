-module(dugong_comm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("dugong_comm.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    dugong_comm_sup:start_link().

stop(_State) ->
    ok.
