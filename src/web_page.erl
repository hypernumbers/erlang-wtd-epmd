%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       The basic web handler for the epmd-a-like web proxy
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(web_page).

-behaviour(cowboy_http_handler).

-include("registry.hrl").
-include_lib("laredo/include/laredo.hrl").

-define(HEAD, "Management Console").
-define(STRAP, "for an (Erlang) WTD EPMD/Proxy").

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

    Header = laredo_bootstrap3:hero(?HEAD, ?STRAP),
    Hdr    = #webpanel{content = Header},
    Panel  = #webpanel{content = list_available()},
    Body   = #webbody{mainbody = Panel,
                      header   = Hdr},
    Page   = #webpage{template = laredo_epmd,
                      webbody = Body},
    HTML   = laredo_api:render_page(Page),
    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

handle_post(Req, State) ->
    http_utils:'404'(?HEAD, ?STRAP, Req, State).

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

list_available() ->
    List = registry:list_availableDB(),
    Header = html:tr([
                 html:th("Name"),
                 html:th("Verified"),
                 html:th("Available"),
                 html:th("Public"),
                 html:th("Private")
                ]),
    TabBody = [html:tr([
                   html:td(N),
                   html:td(atom_to_list(V)),
                   html:td(atom_to_list(A)),
                   html:td(PubK),
                   html:td(PrivK)
                  ])
               || #registry{name         = N,
                            verified     = V,
                            is_available = A,
                            public_key   = PubK,
                            private_key  = PrivK} <- List],
    Content = lists:flatten([Header, TabBody]),
    _HTML = html:table(Content, [], "table table-striped").

