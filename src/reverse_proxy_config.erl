-module(reverse_proxy_config).
-export([init/0]).
-define(TABLE_ID, ?MODULE).
-define(CONFIG_FILE, "reverse_proxy.cfg").

init() ->
    %% TODO write eunit tests
    {ok, Config} = file:consult(?CONFIG_FILE),
    ets:new(?TABLE_ID, [bag, named_table]),
    ets:insert(?TABLE_ID, Config).

%%other processes can lookup the table with:
%%ets:lookup(CONFIG_FILE, "127.0.0.1").
