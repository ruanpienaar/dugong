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
        {udp, Socket, Host, Port, Bin = <<1, Pin:8, Value:8>>} ->
            % pin value changed ( do the db insert here )
            io:format("[PIN VALUE CHANGED] Host : ~p Port ~p Pin ~p received:~p~n",
                [Host, Port, Pin, Bin]),
            loop(Socket);
        {udp, Socket, Host, Port, Bin = <<2, 0:8, 0:8>>} ->
            % device connected
            io:format("[DEVICE CONNECTED] Host : ~p Port ~p received:~p~n",[Host, Port, Bin]),
            loop(Socket);
        {udp, Socket, Host, Port, Bin} ->
            io:format("Host : ~p Port ~p received:~p~n",[Host, Port, Bin]),
            loop(Socket)
    after
        1000 ->
            % io:format(".", []),
            loop(Socket)
    end.

% @doc Send data to the client ( device )
% @end
% 10.151.0.20

device_client(Host, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, Host, ?DEVICE_SERVER_PORT, Data),
    ok = gen_udp:close(Socket).
