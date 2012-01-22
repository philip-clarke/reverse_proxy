-module(reverse_proxy_tests).
-compile([debug_info]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The server will listen on a single port for remote TCP connections

%% A remote TCP connection will spawn a new process to listen for new connections

%% A remote proxy connection will forward traffic to a local port

%% Traffic to the local port will be routed to the remote proxy connection

%% The server will have a registered name so that it can be contacted without tracking its pid ???

 
 
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
 
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
