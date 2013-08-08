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

    Body = [
            #dv{contents = header},
            #dv{class    = "row-fluid",
                contents = [
                            #dv{class    = "span8 clearfix",
                                contents = mainbody},
                            #dv{class    = "span4",
                                contents = sidebar1}
                           ]},
            #dv{class    = "span12 clearfix",
                contents = footer}
           ],

    [
     #dv{class    = "container-fluid",
         contents = [
                     #dv{id       = "content",
                         class    = "clearfix row-fluid",
                         contents = [
                                     #dv{id       = "main",
                                         role     = "main",
                                         contents = Body}
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

javascript_foot() -> [
                      "/_assets/jquery/js/jquery-1.10.2.min.js",
                      "/_assets/bootstrap/js/bootstrap.js",
                      "/_assets/laredo/js/laredo.js"
                     ].

css() -> [
          "/_assets/bootstrap/css/bootstrap.css",
          "/_assets/bootstrap/css/bootstrap-responsive.css"
         ].

%%%-----------------------------------------------------------------------------
%%%
%%% Panel defaults
%%%
%%%-----------------------------------------------------------------------------
header() -> Header = laredo_bootstrap3:hero("Management Console",
                                           "for an (Erlang) WTD EPMD/Proxy"),
            #webpanel{content_type = html,
                      content      = Header}.

navigation() -> none.

mainbody() -> none.

search() -> none.

sidebar1() ->
    Field1 = laredo_bootstrap3:form_field("Email", "Enter your email...",
                                          "We will send you a security key " ++
                                              " (and nothing else)"),
    Form   = laredo_bootstrap3:form("Register your server(s)", [Field1],
                                    "Give Me Superpowers!"),
    Form2  = laredo_bootstrap3:well(Form),
    #webpanel{content_type = html,
              content      = Form2}.

sidebar2() -> none.

sidebar3() -> none.

adverts1() -> none.

adverts2() -> none.

adverts3() -> none.

footer() -> #webpanel{content_type = html,
                      content      = "<div class='muted'>Source code available from <a href='http://github.com/hypernumbers/erlang-wtd-epmd'>Github</a>. This is a management, proxy and epmd server for <a href='http://github.com/hypernumbers/erlang-wtd'>(Erlang)WTD.</a></div>"}.
