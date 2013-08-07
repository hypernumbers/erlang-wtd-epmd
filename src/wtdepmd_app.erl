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
    application:start(wtdepmd),
    application:start(mnesia),
    Debug = case application:get_env(wtdepmd, startup_debug) of
                {ok, true} -> true;
                _          -> false
            end,
    ok = start_mnesia(Debug),
    ok = start_web_gui(Debug).

start(_StartType, _StartArgs) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    wtdepmd_sup:start_link().

stop(_State) ->
    ok.

start_web_gui(Debug) ->
    msg(Debug, "WTD EPMD Debug: Starting web gui~n"),
    {ok, Port} = application:get_env(wtdepmd, web_page_port),
    Dir        = {directory, epmd_utils:get_www_root() ++ "_assets/"},

    Mimetypes = {mimetypes, {fun mimetypes:path_to_mimes/2, default}},

    D = [
         {?ALLHOSTS, [
                      {?ASSETS,   cowboy_static, [Dir, Mimetypes]},
                      {?ALLPATHS, web_page,      []}]}
        ],

    CDispatch = cowboy_router:compile(D),

    {ok, _PID} = cowboy:start_http(web_page_listener, 100,
                                   [{port, Port}],
                                   [{env, [{dispatch, CDispatch}]}]),
    ok.

ensure_dir(Debug) ->
    msg(Debug, "WTD EPMD Debug: make sure the mnesia directory exists...~n"),
    {ok, Dir} = application:get_env(mnesia, dir),
    filelib:ensure_dir(Dir).

start_mnesia(Debug) ->
    ok = ensure_dir(Debug),
    ok = ensure_schema(Debug),
    ok = maybe_create_table(Debug),
    ok.

ensure_schema(Debug) ->
    case mnesia:system_info(tables) of
        [schema] ->
            msg(Debug, "WTD EPMD Debug: plain schema~n"),
            build_schema();
        Tables ->
            msg(Debug, "WTD EPMD Debug: starting tables~n-~p~n", [Tables]),
            mnesia:wait_for_tables(Tables, infinity)
    end,
    ok.

build_schema() ->
    ok = application:stop(mnesia),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia).

maybe_create_table(Debug) ->
    Name = registry,
    TblDef = [
              {record_name,      registry},
              {attributes,       record_info(fields, registry)},
              {disc_only_copies, [node()]},
              {type,             set},
              {local_content,    true},
              {index,            [public_key]}
             ],
    case mnesia:create_table(Name, TblDef) of
        {atomic, ok} ->
            msg(Debug, "WTD EPMD Debug: table created~n");
        {aborted, {already_exists, _}} ->
            msg(Debug, "WTD EPMD Debug: table exists~n");
        {aborted, Reason} ->
            throw(Reason)
    end.

msg(false, _)  -> ok;
msg(true, Msg) -> io:format(Msg).

msg(false, _, _)     -> ok;
msg(true, Msg, Vals) -> io:format(Msg, Vals).
