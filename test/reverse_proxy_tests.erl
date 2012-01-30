-module(reverse_proxy_tests).
-compile([debug_info]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test_() ->
    {"The server can be started and will listen on 4590 for a tcp connection",
        {setup,
         fun start/0,
         fun stop/1,
         fun (SetupData) ->
            [can_start(SetupData),
             test_listening(SetupData)]
         end}}.


stop_test_() ->
    {"The server can be stopped",
        {setup,
         fun start/0,
         fun can_stop/1}}.


%% A remote proxy connection will forward traffic to a local port

%% Traffic to the local port will be routed to the remote proxy connection

%% The server will have a registered name so that it can be contacted without tracking its pid ???

 
 
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    {ok, Pid} = reverse_proxy:start_link(),
    Pid.


stop(Pid) ->
    wait_for_exit(Pid).

 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

can_start(Pid) ->
    [?_assert(erlang:is_process_alive(Pid))].


can_stop(Pid) ->
    %% because the server is blocking and waiting for a connection, we have to connect before the stop will be processed
    PortNum = 4950,
    gen_tcp:connect("localhost", PortNum, [binary, {packet, 0}]),
    MRef = erlang:monitor(process, Pid),
    reverse_proxy:stop(Pid),
    Rec = receive 
        {'DOWN', MRef, process, Pid, normal} -> true;
        Other -> 
            ?debugFmt("can_stop received ~p~n", [Other]),
            false
    end,
    [?_assert(Rec)].


test_listening(_Pid) ->
    Hostname = "localhost",
    PortNum = 4950,
    [?_assertMatch({ok, _Sock}, gen_tcp:connect(Hostname, PortNum, [binary, {packet, 0}]))].

 
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
