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
    Panel = #webpanel{content = "<h1>Howdy Hombre</h1>"},
    Body = #webbody{mainbody = Panel},
    Page = #webpage{template = laredo_epmd, webbody = Body},
    HTML = laredo_api:render_page(Page),
    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
