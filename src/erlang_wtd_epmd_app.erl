-module(erlang_wtd_epmd_app).

-behaviour(application).

%% Application callbacks
-export([
         start/0, start/2,
         stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() -> start([], []).

start(_StartType, _StartArgs) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    erlang_wtd_epmd_sup:start_link().

stop(_State) ->
    ok.
