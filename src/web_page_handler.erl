%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       The basic web handler for the epmd-a-like web proxy
%%%
%%% @end
%%% Created : 26 Jul 2013 by gordonguthrie@backawinner.gg

-module(web_page_handler).

-behaviour(cowboy_http_handler).

-include("registry.hrl").
-include("epmd_srv.hrl").
-include_lib("laredo/include/laredo.hrl").

-define(HEAD,  "Management Console").
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

    Content = html:h3("Connected Nodes") ++
        list_connected() ++
        html:h3("Available Nodes") ++
        list_available(),

    Header = laredo_bootstrap3:hero(?HEAD, ?STRAP),
    Hdr    = #webpanel{content = Header},
    Panel  = #webpanel{content = Content},
    Body   = #webbody{mainbody = Panel,
                      header   = Hdr},
    Page   = #webpage{template = laredo_epmd,
                      webbody  = Body},
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
    List = registry:list_entriesDB(),
    Header = html:tr([
                 html:th("Public"),
                 html:th("Private"),
                 html:th("Verified")
                ]),
    TabBody = [html:tr([
                   html:td(PubK),
                   html:td(PrivK),
                   html:td(atom_to_list(V))
                  ])
               || #registry{verified     = V,
                            public_key   = PubK,
                            private_key  = PrivK} <- List],
    Content = lists:flatten([Header, TabBody]),
    _HTML = html:table(Content, [], "table table-striped").

list_connected() ->
    MakeBodyFn
        = fun(Name, Records) ->
                  Nm   = epmd_utils:to_s(Name),
                  Recs = [make_def(X) || X <- Records],
                  [html:tr([
                            html:td(Nm),
                            html:td(Recs)
                           ])]
          end,
    {servers, List} = epmd_srv:get_servers(),
    Header = html:tr([
                 html:th("Server"),
                 html:th("Missions")
                ]),
    TabBody = [MakeBodyFn(Name, Rs) ||  {Name, {Rs, _}} <- List],
    Content = lists:flatten([Header, TabBody]),
    _HTML = html:table(Content, [], "table table-striped").

make_def(#mission{name       = Name,
                  public_key = PubK,
                  exports    = Exps,
                  behaviours = Bhvs}) ->
    Defs = [
            {"Name",       Name},
            {"Public Key", epmd_utils:to_s(PubK)},
            {"Exports",    [epmd_utils:to_s(X) || X <- Exps]},
            {"Behaviours", [epmd_utils:to_s(X) || X <- Bhvs]}
           ],
    laredo_bootstrap3:definitions(Defs).

