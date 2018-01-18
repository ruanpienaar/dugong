-module(dugong_write_fsm_sup).
-behavior(supervisor).

-export([start_write_fsm/1, start_link/0]).
-export([init/1]).

-ignore_xref([init/1, start_link/0]).

start_write_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WriteFsm = {undefined,
                {dugong_write_fsm, start_link, []},
                temporary, 5000, worker, [dugong_write_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [WriteFsm]}}.
