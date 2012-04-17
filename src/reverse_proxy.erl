%%%-------------------------------------------------------------------
%%% @author philip
%%% @copyright (C) 2012, philip
%%% @doc
%%%
%%% @end
%%% Created : 2012-01-23 21:37:28.316943
%%%-------------------------------------------------------------------
-module(reverse_proxy).
-compile([debug_info]).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        remotePort = 4900, 
        remoteListenSock, % a ListenSocket on which may be passed into gen_tcp accept/1 
        remoteSock,   % a socket() through which the remote client can send/receive data
        localPort = 4950, 
        localHost = "localhost",
        localSock   % a socket() through which the local application can send/receive data
    }
).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_server:start_link(?MODULE, [], []).

stop(_Pid) ->
        gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        State = #state{},
        %% TODO creating the socket will be done in the application code later on
        %% TODO a configuration file should determine which remote ip addresses
        %% are mapped to which local port
        {ok, LSock} = gen_tcp:listen(State#state.remotePort, [binary, {active, true}, {packet, 0}, {reuseaddr, true}]),
        NewState = State#state{remoteListenSock = LSock},
        io:format("~p~n", [NewState]),
        {ok, NewState, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
        ok = gen_tcp:close(State#state.remoteListenSock),
        close(State#state.remoteSock),
        close(State#state.localSock),
        {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
        LSock = State#state.remoteListenSock,
        %% TODO but we should also listen on the localPort
        {ok, RemoteSock} = gen_tcp:accept(LSock), %% NB this call will block
        {ok, LocalSock} = gen_tcp:connect(State#state.localHost, State#state.localPort, [binary, {packet, 0}]),
        %% now data can be passed between localSock and remoteSock
        NewState = State#state{remoteSock = RemoteSock, localSock = LocalSock},
        %io:format("~p~n", [NewState]),
        {noreply, NewState};


handle_info({tcp, _Port, Data}, State) ->
        %io:format("received ~p~n", [Data]),
        ok = gen_tcp:send(State#state.localSock, Data),
        {noreply, State};

handle_info({tcp_closed, _Sock}, State) ->
        %io:format("tcp_closed received for ~p~n", [Sock]),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
        %% TODO make sure that the sock is not undefined before trying to close
        close(State#state.remoteListenSock),
        close(State#state.remoteSock),
        close(State#state.localSock),
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
close(Sock) when Sock /= undefined ->
    gen_tcp:close(Sock);
close(_Sock) ->
    ok.
