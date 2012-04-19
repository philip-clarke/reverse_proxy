-module(reverse_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% TODO for now, the reverse_proxy_config is not supervised (see Ch 6 Erlang and OTP in Action)
    reverse_proxy_config:init(),
    reverse_proxy_sup:start_link().

stop(_State) ->
    ok.
