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

    Panel = #webpanel{content = list_available()},
    Body  = #webbody{mainbody = Panel},
    Page  = #webpage{template = laredo_epmd, webbody = Body},
    HTML  = laredo_api:render_page(Page),
    {ok, Req2} = cowboy_req:reply(200, [], HTML, Req),
    {ok, Req2, State}.

handle_post(_Req, _State) ->
    io:format("Got a post...~n"),
    ok.

terminate(_Reason, _Req, _State) ->
    ok.

%%%
%%% Internal Fns
%%%

list_available() ->
    List = registry:list_availableDB(),
    Header = tr([
                 th("Name"),
                 th("Verified"),
                 th("Available"),
                 th("Public"),
                 th("Private")
                ]),
    TabBody = [tr([
                   td(N),
                   td(V),
                   td(A),
                   td(PubK),
                   td(PrivK)
                  ])
               || #registry{name         = N,
                            verified     = V,
                            is_available = A,
                            public_key   = PubK,
                            private_key  = PrivK} <- List],
    _HTML = "<table class='table table-striped'>"
        ++ lists:flatten([Header, TabBody])
        ++ "</table>".

th(X) when is_atom(X) -> "<th>" ++ atom_to_list(X) ++ "</th>";
th(X) when is_list(X) -> "<th>" ++ X ++ "</th>".

td(X) when is_atom(X) -> "<td>" ++ atom_to_list(X) ++ "</td>";
td(X) when is_list(X) -> "<td>" ++ X ++ "</td>".

tr(X) when is_list(X) -> "<td>" ++ X ++ "</tr>".
