%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(wtdepmd_app).

-behaviour(application).

-include("registry.hrl").

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
    ok.

start(_StartType, _StartArgs) ->
    DebugOn = case application:get_env(wtdepmd, startup_debug) of
                  {ok, true} -> true;
                  _          -> false
              end,
    ok = start_mnesia(DebugOn),
    ok = start_web_gui(DebugOn),
    ok = start_proxy(DebugOn),
    ok = start_epmd(DebugOn),
    wtdepmd_sup:start_link().

stop(_State) ->
    ok.

start_web_gui(DebugOn) ->

    msg(DebugOn, "WTD EPMD Debug: Starting web gui~n"),

    {ok, WebPort}   = application:get_env(wtdepmd, web_page_port),

    AssetDirective = get_asset_directive(),

    D = [
         {?ALLHOSTS, [
                      AssetDirective,
                      {?ALLPATHS, web_page_handler, []}]}
        ],


    CDispatch = cowboy_router:compile(D),

    {ok, _PID} = cowboy:start_http(web_page_listener, 100,
                                   [{port, WebPort}],
                                   [{env, [{dispatch, CDispatch}]}]),
    ok.

start_proxy(DebugOn) ->

    msg(DebugOn, "WTD EPMD Debug: starting proxy~n"),

    {ok, ProxyPort} = application:get_env(wtdepmd, wtd_proxy_port),

    AssetDirective = get_asset_directive(),

    D = [
         {?ALLHOSTS, [
                      AssetDirective,
                      {?ALLPATHS, wtd_proxy_handler, []}]}
        ],

    CDispatch = cowboy_router:compile(D),
    {ok, _PID} = cowboy:start_http(proxy_listener, 100,
                                   [{port, ProxyPort}],
                                   [{env, [{dispatch, CDispatch}]}]),
    ok.

start_epmd(DebugOn) ->

    msg(DebugOn, "WTD EPMD Debug: starting epmd~n"),

    {ok, EPMDPort}  = application:get_env(wtdepmd, wtd_epmd_port),

    AssetDirective = get_asset_directive(),

    D = [
         {?ALLHOSTS, [
                      AssetDirective,
                      {?ALLPATHS, epmd_handler, []}]}
        ],

    CDispatch = cowboy_router:compile(D),
    {ok, _PID} = cowboy:start_http(epmd_listener, 100,
                                   [{port, EPMDPort}],
                                   [{env, [{dispatch, CDispatch}]}]),
    ok.

ensure_dir(DebugOn) ->

    msg(DebugOn, "WTD EPMD Debug: make sure the mnesia directory exists...~n"),

    {ok, Dir} = application:get_env(mnesia, dir),
    filelib:ensure_dir(Dir).

start_mnesia(DebugOn) ->
    ok = ensure_dir(DebugOn),
    ok = ensure_schema(DebugOn),
    ok = maybe_create_table(DebugOn),
    ok.

ensure_schema(DebugOn) ->
    case mnesia:system_info(tables) of
        [schema] ->
            msg(DebugOn, "WTD EPMD Debug: plain schema~n"),
            build_schema();
        Tables ->
            msg(DebugOn, "WTD EPMD Debug: starting tables~n-~p~n", [Tables]),
            mnesia:wait_for_tables(Tables, infinity)
    end,
    ok.

build_schema() ->
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia).

maybe_create_table(DebugOn) ->
    Name = registry,
    TblDef = [
              {record_name,      registry},
              {attributes,       record_info(fields, registry)},
              {disc_only_copies, [node()]},
              {type,             set},
              {local_content,    true}
             ],
    case mnesia:create_table(Name, TblDef) of
        {atomic, ok} ->
            msg(DebugOn, "WTD EPMD Debug: table created~n");
        {aborted, {already_exists, _}} ->
            msg(DebugOn, "WTD EPMD Debug: table exists~n");
        {aborted, Reason} ->
            throw(Reason)
    end.

msg(false, _)  -> ok;
msg(true, Msg) -> io:format(Msg).

msg(false, _, _)     -> ok;
msg(true, Msg, Vals) -> io:format(Msg, Vals).

get_asset_directive() ->
    Dir       = {directory, epmd_utils:get_www_root() ++ "_assets/"},
    Mimetypes = {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
    {?ASSETS,   cowboy_static, [Dir, Mimetypes]}.


