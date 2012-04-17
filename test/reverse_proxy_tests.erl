-module(reverse_proxy_tests).
-compile([debug_info]).
-include_lib("eunit/include/eunit.hrl").

%% TODO is there a way to import state from the module
-record(state, {
        remotePort = 4900, 
        listenSock,
        remoteSock,   % connects to the remotePort
        localPort = 4950, 
        localHost = "localhost",
        localSock   % a socket() through which the local application can send/receive data
    }
).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test_() ->
    {"The server can be started and will listen on 4900 for a tcp connection",
     {setup,
      fun start/0,
      fun stop/1,
      fun (SetupData) ->
        [test_start(SetupData),
         test_listening(SetupData)]
            end}}.


stop_test_() ->
    {"The server can be stopped",
     {setup,
      fun start/0,
      fun test_stop/1}}.


forwarding_test_() ->
    {"A remote proxy port will forward traffic to a local port",
     {setup,
      fun start/0,
      fun stop/1,
      fun test_forwarding/1}}.

%% a connection to the remote port is accepted if the remote ip address is white listed

%% a connection to the remote port is rejected if the remote ip address is not white listed

%% remote packets are silently dropped if there is no local application socket open

%% a connection from the local application is rejected if there is no remote socket open for communication

%% when the remote socket is closed, the application socket will be closed

%% a process related to a remote socket can be restarted
 
 
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    TestState = #state{},
    LocalPortNum = TestState#state.localPort,
    {ok, LSock} = gen_tcp:listen(LocalPortNum, [binary, {packet, 0}, {active, true}]),
    ?debugHere,
    {ok, Pid} = reverse_proxy:start_link(),
    {Pid, TestState#state{listenSock = LSock}}.


stop({Pid, TestState}) ->
    gen_tcp:close(TestState#state.listenSock),
    wait_for_exit(Pid).

 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

test_start({Pid, TestState}) ->
    ?debugFmt("~p~n", [TestState]),
    [?_assert(erlang:is_process_alive(Pid))].


test_listening({_Pid, TestState}) ->
    ?debugFmt("~p~n", [TestState]),
    timer:sleep(1000),
    {Res, Sock} = gen_tcp:connect(TestState#state.localHost, TestState#state.remotePort, [binary, {packet, 0}]),
    gen_tcp:close(Sock),
    [?_assertMatch(ok, Res)].


test_stop({Pid, TestState}) ->
    ?debugFmt("~p~n", [TestState]),
    %% because the server is blocking and waiting for a connection, we have to connect before the stop will be processed
    HostName = TestState#state.localHost,
    PortNum = TestState#state.remotePort,
    {ok, Sock} = gen_tcp:connect(HostName, PortNum, [binary, {packet, 0}]),
    %%?debugFmt("reverse_proxy: ~p~n", [sys:get_status(Pid)]),
    gen_tcp:close(Sock),
    MRef = erlang:monitor(process, Pid),
    ?debugFmt("test_stop: ~p~n", [TestState]),
    reverse_proxy:stop(Pid),
    Rec = 
    receive 
        {'DOWN', MRef, process, Pid, normal} -> 
            true;
        Other2 -> 
            ?debugFmt("test_stop received ~p~n", [Other2]),
            {_, S} = Other2,
            ?debugFmt("~p~n", [erlang:port_info(S)]),
            false
    end,
    gen_tcp:close(TestState#state.listenSock),
    [?_assert(Rec)].


test_forwarding({_Pid, TestState}) ->
    ?debugFmt("~p~n", [TestState]),
    timer:sleep(1000),
    {ok, Sock} = gen_tcp:connect(TestState#state.localHost, TestState#state.remotePort, [binary, {packet, 0}]),
    {ok, S} = gen_tcp:accept(TestState#state.listenSock),
    ok = gen_tcp:send(Sock, "Hello World"),
    %{ok, Packet} = gen_tcp:recv(S, 0, 5000), %% 5 second timeout, but only used for passive sockets
    Res = 
    receive
            {tcp, Port, Data} -> {tcp, Port, Data}
    end,
    gen_tcp:close(Sock),
    [?_assertEqual(<<"Hello World">>, Data)].

 
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    reverse_proxy:stop(Pid),
    receive 
        {'DOWN', MRef, _, _, _} -> 
            ok;
        Other ->
            ?debugFmt("wait_for_exit ~p~n", [Other])
    end.
