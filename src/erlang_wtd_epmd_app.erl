%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(erlang_wtd_epmd_app).

-behaviour(application).

%% Application callbacks
-export([
         start/0, start/2,
         stop/1
        ]).

-define(ALLHOSTS, '_').
-define(ALLPATHS, '_').

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() -> start([], []).

start(_StartType, _StartArgs) ->

    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),

    % Ret = application:get_env(erlang_wtd_epmd, web_page_port),
    % io:format("application:get_env returns ~p~n", [Ret]),
    % Ret2 = application:get_all_env(),
    % io:format("application:get_all_env returns ~p~n", [Ret2]),
    %{ok, Port} = Ret,

    Port = 14151,

    Dispatch = cowboy_router:compile([{?ALLHOSTS, [{?ALLPATHS, web_page, []}]}]),
    CDispatch = cowboy_router:compile(Dispatch),

    {ok, _PID} = cowboy:start_http(web_page_listener, 100,
                           [{port, Port}],
                           [{env, [{dispatch, CDispatch}]}]),

    erlang_wtd_epmd_sup:start_link().

stop(_State) ->
    ok.
