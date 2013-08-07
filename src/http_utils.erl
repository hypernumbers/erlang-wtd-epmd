%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, vagrant
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2013 by gordonguthrie@backawinner.gg

-module(http_utils).

-export([
         matches_json/1,
         '404'/4
        ]).

-include_lib("laredo/include/laredo.hrl").

matches_json(Content) when is_binary(Content) ->
    Components = binary:split(Content, <<";">>),
    matches_j2(Components).

matches_j2([])                            -> false;
matches_j2([<<"application/json">> | _T]) -> true;
matches_j2([_H                     | T])  -> matches_j2(T).

'404'(Head, Strap, Req, State) ->
    Header = laredo_bootstrap3:hero(Head, Strap),
    HeaderPanel = #webpanel{content = Header},
    Panel = #webpanel{content = html:h1("404 - page not found")},
    Body  = #webbody{mainbody = Panel,
                     header   = HeaderPanel,
                     sidebar1 = #webpanel{}},
    Page  = #webpage{template = laredo_epmd, webbody = Body},
    HTML  = laredo_api:render_page(Page),
    {ok, Req2} = cowboy_req:reply(404, [], HTML, Req),
    {ok, Req2, State}.
