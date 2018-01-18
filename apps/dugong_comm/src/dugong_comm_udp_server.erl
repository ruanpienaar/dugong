-module(dugong_comm_udp_server).

%% API calls
-export([
    start_link/0,
    device_client/2
]).

-define(DB_SERVER_PORT, 4000).
-define(DEVICE_SERVER_PORT, 4001).

%% Exported or proc_lib
-export([
    do_start_link/1
]).

% @doc Server on the db side ( receive traffic from devices )
% @end
start_link() ->
    % {ok, proc_lib:start_link(?MODULE, do_start_link, [{4000}])}.
    proc_lib:start_link(?MODULE, do_start_link, [{?DB_SERVER_PORT}]).

do_start_link({Port}) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
    io:format("server opened socket:~p~n",[Socket]),
    ok = proc_lib:init_ack({ok, self()}),
    loop(Socket).

loop(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Host, Port, Bin} ->
            io:format("server received:~p~n",[Bin]),
            ok = gen_udp:send(Socket, Host, Port, Bin),
            loop(Socket)
    after
        1000 ->
            % io:format(".", []),
            loop(Socket)
    end.

% @doc Send data to the client ( device )
% @end
device_client(Host, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, Host, ?DEVICE_SERVER_PORT, Data),
    Value = receive
                {udp, Socket, _, _, Bin} ->
                    io:format("client received:~p~n",[Bin])
            after 1000 ->
                    0
            end,
    ok = gen_udp:close(Socket),
    Value.
