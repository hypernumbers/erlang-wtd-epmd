%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Handles epmd connections
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(epmd_handler).

-behaviour(cowboy_http_handler).

-include("registry.hrl").
-include_lib("laredo/include/laredo.hrl").

-define(HEAD,  "EPMD Server").
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
    {Path, _}    = cowboy_req:path(Req),
    case Path of
        <<"/">> -> handle_p(Req, State);
        _       -> http_utils:'404'(?HEAD, ?STRAP, Req, State)
    end.

handle_p(Req, State) ->
    {ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
    {Accept, _}      = cowboy_req:header(<<"accept">>, Req),
    Hdrs = [{<<"content-type">>, ContentType}],
    JSONIn  = http_utils:matches_json(ContentType),
    JSONOut = http_utils:matches_json(Accept),
    case {JSONIn, JSONOut} of
        {true, true} -> handle_p2(Hdrs, Req, State);
        {_,    _}    -> http_utils:'404'(?HEAD, ?STRAP, Hdrs, Req, State)

    end.

handle_p2(Hdrs, Req, State) ->
    PublicKey = epmd_utils:get_public_key(Req),
    case registry:lookup(PublicKey) of
        {error, no_key} ->
            Resp = {[{error, not_registered}]},
            http_utils:'403'(Resp, Hdrs, Req, State);
        {ok, #registry{verified = false}} ->
            Resp = {[{error, not_verified}]},
            http_utils:'403'(Resp, Hdrs, Req, State);
        {ok, #registry{private_key = PrivateKey, verified = true}} ->
            handle_p3(PublicKey, PrivateKey, Hdrs, Req, State)
    end.

handle_p3(PublicKey, PrivateKey, Hdrs, Req, State) ->
    IsAuth = hmac_api_lib:cowboy_authorize_request(Req, PublicKey, PrivateKey),
    case IsAuth of
        "match" ->
            Resp = {[{ok, authenticated}]},
            {ok, Binary, _} = cowboy_req:body(Req),
            {Body2} = jiffy:decode(Binary),
            {_, Name} = lists:keyfind(<<"name">>, 1, Body2),
            {_, Vals} = lists:keyfind(<<"missions">>, 1, Body2),
            ok = epmd_srv:got_ping({PublicKey, Name}, Vals),
            http_utils:'200'(Resp, Hdrs, Req, State);
        "nomatch" ->
            Resp = {[{error, denied}]},
            http_utils:'403'(Resp, Hdrs, Req, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

