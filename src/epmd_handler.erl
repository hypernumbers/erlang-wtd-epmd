%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Handles epmd connections
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(epmd_handler).

-behaviour(cowboy_http_handler).

-include("registry.hrl").
-include("wtd.hrl").
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
    {ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
    {Accept, _}      = cowboy_req:header(<<"accept">>, Req),
    Hdrs = [{<<"content-type">>, ContentType}],
    BERTIn  = http_utils:matches_bert(ContentType),
    BERTOut = http_utils:matches_bert(Accept),
    case {BERTIn, BERTOut} of
        {true, true} -> handle_p2(Hdrs, Req, State);
        {_,    _}    -> http_utils:'404'(?HEAD, ?STRAP, Hdrs, Req, State)

    end.

handle_p2(Hdrs, Req, State) ->
    PublicKey = epmd_utils:get_public_key(Req),
    case registry:lookup(PublicKey) of
        {error, no_key} ->
            Resp = {error, "not_registered"},
            http_utils:'403'(Resp, Hdrs, Req, State);
        {ok, #registry{verified = false}} ->
            Resp = {error, "not_verified"},
            http_utils:'403'(Resp, Hdrs, Req, State);
        {ok, #registry{private_key = PrivateKey, verified = true}} ->
            handle_p3(PublicKey, PrivateKey, Hdrs, Req, State)
    end.

handle_p3(PublicK, PrivateK, Hdrs, Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    IsAuth    = hmac_api_lib:cowboy_authorize_request(Req, PublicK, PrivateK),
    case IsAuth of
        "match" ->
            case Path of
                <<"/">>    -> handle_epmd(Hdrs, Req, State);
                <<"/tx/">> -> handle_transmission(Hdrs, Req, State);
                <<"/rx/">> -> handle_receive(Hdrs, Req, State)
            end;
        "nomatch" ->
            Resp = {error, "denied"},
            http_utils:'403'(Resp, Hdrs, Req, State)
    end.

handle_epmd(Hdrs, Req, State) ->
            {ok, Binary, _}  = cowboy_req:body(Req),
            {Name, Missions} = bert:decode(base64:decode(Binary)),
            Resp             = {ok, epmd_srv:got_ping(Name, Missions)},
            http_utils:'200'(Resp, Hdrs, Req, State).

handle_transmission(Hdrs, Req, State) ->
    {ok, Binary, _} = cowboy_req:body(Req),
    Msg = bert:decode(base64:decode(Binary)),
    #signed_request{public_key = PubK,
                    signature  = Sig,
                    request    = RemoteReq} = Msg,
    io:format("PubK is ~p~nSig is ~p~nRemoteReq is ~p~n",
              [PubK, Sig, RemoteReq]),
    #request{node      = Node,
             module    = Mod,
             function  = Fn,
             arguments = Args,
             date      = Date} = RemoteReq,
    io:format("Node is ~p~nMod is ~p~nFn is ~p~nArgs is ~p~nDate is ~p~n",
              [Node, Mod, Fn, Args, Date]),
    Reply = cache_srv:tx(Node, Msg),
    http_utils:'200'(Reply, Hdrs, Req, State).

handle_receive(Hdrs, Req, State) ->
    {ok, Binary, _} = cowboy_req:body(Req),
    Server = bert:decode(base64:decode(Binary)),
    Timeout = cache_srv:rx(Server, self()),
    io:format("Timeout is ~p~n", [Timeout]),
    {Status, R} = receive
                      {tcp_closed, _Socket} -> {close, ok};
                      {error, timeout}      -> {ok, timeout};
                      {msg, Data}           -> {ok, Data}
                  after
                      Timeout -> {ok, {timeout, now()}}
                  end,
    case Status of
        ok    -> http_utils:'200'(R, Hdrs, Req, State);
        close -> ok
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

