%%%-------------------------------------------------------------------
%%% @author philip
%%% @copyright (C) 2012, philip
%%% @doc
%%%
%%% @end
%%% Created : 2012-02-21 21:02:46.025592
%%%-------------------------------------------------------------------
-module(reverse_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(LISTEN_PORT, 4900).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        %% this callback will start up a number of children as well as returning the spec
        %% the number of children will be the number of entries in the config file
        %% otherwise 5 children will be started.  The config file is read in by the application
        RestartStrategy = simple_one_for_one,
        MaxRestarts = 1000,
        MaxSecondsBetweenRestarts = 3600,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = permanent,
        Shutdown = 2000,
        Type = worker,

        {ok, ListenSock} = gen_tcp:listen(?LISTEN_PORT, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
        AChild = {reverse_proxy, 
                  {reverse_proxy, start_link, [ListenSock]},
                  Restart, Shutdown, Type, [reverse_proxy]},

        spawn_link(fun start_all_children/0),
        {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_child() ->
    supervisor:start_child(reverse_proxy_sup, []).


start_all_children() ->
    NumConnections = 
    case ets:info(reverse_proxy_config, size) of
        undefined ->  5;
        N -> N
    end,
        
    io:format("~p~n", [NumConnections]),
    [start_child() || _ <- lists:seq(1, NumConnections)],
    ok.


