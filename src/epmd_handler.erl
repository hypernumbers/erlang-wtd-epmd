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

-define(publickey,  "0PN5J17HBGZHT7JJ3X82").
-define(privatekey, "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o").

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
    IsAuthenticated  = hmac_api_lib:cowboy_authorize_request(Req, ?publickey,
                                                             ?privatekey),
    Hdrs = [{<<"content-type">>, ContentType}],
    JSONIn  = http_utils:matches_json(ContentType),
    JSONOut = http_utils:matches_json(Accept),
    case {JSONIn, JSONOut, IsAuthenticated} of
        {true, true, "match"} ->
            Resp = "{'ok': 'ya bas'}",
            PublicKey = epmd_utils:get_public_key(Req),
            epmd_srv:got_ping(PublicKey),
            {ok, Req2} = cowboy_req:reply(200, Hdrs, Resp, Req),
            {ok, Req2, State};
        {true, true, _} ->
            Resp = "{'gtf': 'ya wino'}",
            {ok, Req2} = cowboy_req:reply(403, Hdrs, Resp, Req),
            {ok, Req2, State};
        _            ->
            http_utils:'404'(?HEAD, ?STRAP, Req, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

