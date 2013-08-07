%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Handles proxy connections
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(wtd_proxy).

-behaviour(cowboy_http_handler).

-include("registry.hrl").
-include_lib("laredo/include/laredo.hrl").

-define(HEAD,  "Proxy Server").
-define(STRAP, "for (Erlang) WTD").

-export([
         init/3,
         handle/2,
         terminate/3
        ]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of
        <<"GET">>  -> handle_get(Req,  State);
        <<"POST">> -> handle_post(Req, State)
    end.

handle_get(Req, State) ->
    http_utils:'404'(?HEAD, ?STRAP, Req, State).

handle_post(Req, State) ->
    {ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
    {Accept, _}      = cowboy_req:header(<<"accept">>, Req),
    Resp = "{'response': 'ok'}",
    Hdrs = [{<<"content-type">>, ContentType}],
    case {http_utils:matches_json(ContentType),
          http_utils:matches_json(Accept)} of
        {true, true} -> {ok, Req2} = cowboy_req:reply(200, Hdrs, Resp, Req),
                        {ok, Req2, State};
        _            -> http_utils:'404'(?HEAD, ?STRAP, Req, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

