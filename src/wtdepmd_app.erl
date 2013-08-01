%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(wtdepmd_app).

-behaviour(application).

%% Application callbacks
-export([
         start/0, start/2,
         stop/1
        ]).

-define(ALLHOSTS, '_').
-define(ALLPATHS, '_').
-define(ASSETS, "/_assets/[...]").

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    application:start(wtdepmd),

    {ok, Port}   = application:get_env(wtdepmd, web_page_port),
    {ok, WWWDir} = application:get_env(wtdepmd, www_root),

    Dir = {directory, WWWDir},
    Mimetypes = {mimetypes, {fun mimetypes:path_to_mimes/2, default}},

    D = [
         {?ALLHOSTS, [
                      {?ASSETS,   cowboy_static, [Dir, Mimetypes]},
                      {?ALLPATHS, web_page,      []}]}
        ],

    CDispatch = cowboy_router:compile(D),

    {ok, _PID} = cowboy:start_http(web_page_listener, 100,
                                   [{port, Port}],
                                   [{env, [{dispatch, CDispatch}]}]).

start(_StartType, _StartArgs) ->

    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    wtdepmd_sup:start_link().

stop(_State) ->
    ok.
