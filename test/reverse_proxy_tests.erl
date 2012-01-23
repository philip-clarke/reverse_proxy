-module(reverse_proxy_tests).
-compile([debug_info]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start_test_() ->
    {"The server can be started",
        {setup,
         fun start/0,
         fun stop/1,
         fun can_start/1}}.


stop_test_() ->
    {"The server can be stopped",
        {setup,
         fun start/0,
         fun can_stop/1}}.


%% The server will listen on a single port for remote TCP connections

%% A remote TCP connection will spawn a new process to listen for new connections

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
    reverse_proxy:stop(Pid),
    wait_for_exit(Pid).

 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

can_start(Pid) ->
    [?_assert(erlang:is_process_alive(Pid))].


can_stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    reverse_proxy:stop(Pid),
    Rec = receive 
        {'DOWN', MRef, process, Pid, normal} -> true;
        _ -> false
    end,
    [?_assert(Rec)].

 
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end.
