%%% @author        Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc           the laredo templating system
%%%
%%% @end
%%% Created : 28 Jul 2013 by gordon@vixo.com

-module(laredo_epmd).

-include_lib("laredo/include/laredo.hrl").

-define(NOJAVASCRIPT, []).
-define(NOCSS,        []).

%% export for getting the default page structure
-export([
         get_page/0
        ]).

%% exports for making the page defaults
-export([
         title/0,
         language/0,
         meta/0,
         javascript_head/0,
         javascript_foot/0,
         css/0
        ]).

%% exports for making the panel defaults
-export([
         header/0,
         navigation/0,
         mainbody/0,
         search/0,
         sidebar1/0,
         sidebar2/0,
         sidebar3/0,
         adverts1/0,
         adverts2/0,
         adverts3/0,
         footer/0
        ]).

%%%-----------------------------------------------------------------------------
%%%
%%% Default Page
%%%
%%%-----------------------------------------------------------------------------
get_page() ->
    [
     #dv{class    = "container-fluid",
         contents = [
                     #dv{id       = "content",
                         class    = "clearfix row-fluid",
                         contents = [
                                     #dv{id       = "main",
                                         class    = "span12 clearfix",
                                         role     = "main",
                                         contents = [
                                                     #dv{contents = header},
                                                     #dv{contents = mainbody},
                                                     #dv{contents = footer}
                                                    ]}
                                    ]}
                    ]}
    ].

%%%-----------------------------------------------------------------------------
%%%
%%% Page defaults
%%%
%%%-----------------------------------------------------------------------------
title() -> "<title>WTD EMPD Management Console</title>".

language() -> "en".

meta() -> "<meta name='viewport' "
              ++ "content='width=device-width, initial-scale=1.0'>".

javascript_head() -> [].

javascript_foot() -> ["./_assets/js/bootstrap.js"].

css() -> [
          "./_assets/css/bootstrap.css",
          "./_assets/css/bootstrap-responsive.css"
         ].

%%%-----------------------------------------------------------------------------
%%%
%%% Panel defaults
%%%
%%%-----------------------------------------------------------------------------
header() -> Header = laredo_bootstrap3:hero("Management Console",
                                           "for your (Erlang) WTD EPMD Proxy"),
            #webpanel{content_type = html,
                      content      = Header}.

navigation() -> none.

mainbody() -> none.

search() -> none.

sidebar1() -> none.

sidebar2() -> none.

sidebar3() -> none.

adverts1() -> none.

adverts2() -> none.

adverts3() -> none.

footer() -> #webpanel{content_type = html,
                      content      = "<div class='muted'>Source code availalbe from <a href='http://github.com/hypernumbers/erlang-wtd-epmd'>Github</a>. This is a proxy server for <a href-'http://github.com:/hypernumbers/erlang-wtd'>(Erlang)WTD.</div>"}.
