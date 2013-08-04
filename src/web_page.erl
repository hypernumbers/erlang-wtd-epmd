%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       The basic web handler for the epmd-a-like web proxy
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(web_page).

-behaviour(cowboy_http_handler).

-include_lib("laredo/include/laredo.hrl").

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
    Field1 = laredo_bootstrap3:form_field("Email", "Enter your email...",
                                          "We will send you a security key " ++
                                              " (and nothing else)"),
    Form   = laredo_bootstrap3:form("Register your server(s)", [Field1],
                                 "Give Me Superpowers!"),
    Form2  = laredo_bootstrap3:well(Form),

    Panel = #webpanel{content = Form2},
    Body  = #webbody{mainbody = Panel},
    Page  = #webpage{template = laredo_epmd, webbody = Body},
    HTML  = laredo_api:render_page(Page),
    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

handle_post(Req, State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.
