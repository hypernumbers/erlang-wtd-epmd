%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, vagrant
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2013 by gordonguthrie@backawinner.gg

-module(http_utils).

-export([
         matches_json/1,
         '404'/5,
         '403'/4,
         '200'/4
        ]).

-include_lib("laredo/include/laredo.hrl").

matches_json(undefined)           -> false;
matches_json(B) when is_binary(B) -> B2 = binary:split(B, <<";">>),
                                     matches_j2(B2).

matches_j2([])                            -> false;
matches_j2([<<"application/json">> | _T]) -> true;
matches_j2([_H                     | T])  -> matches_j2(T).

'404'(Head, Strap, Headers, Req, State) ->
    Header = laredo_bootstrap3:hero(Head, Strap),
    HeaderPanel = #webpanel{content = Header},
    Panel = #webpanel{content = html:h1("404 - page not found")},
    Body  = #webbody{mainbody = Panel,
                     header   = HeaderPanel,
                     sidebar1 = #webpanel{}},
    Page  = #webpage{template = laredo_epmd, webbody = Body},
    HTML  = laredo_api:render_page(Page),
    {ok, Req2} = cowboy_req:reply(404, Headers, HTML, Req),
    {ok, Req2, State}.

'403'(Response, Headers, Req, State) ->
    Resp = jiffy:encode(Response),
    {ok, Req2} = cowboy_req:reply(403, Headers, Resp, Req),
    {ok, Req2, State}.

'200'(Response, Headers, Req, State) ->
    Resp = jiffy:encode(Response),
    {ok, Req2} = cowboy_req:reply(200, Headers, Resp, Req),
    {ok, Req2, State}.
