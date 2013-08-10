%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(epmd_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% General API
-export([
         get_servers/0,
         got_ping/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(server,
        {
          name,
          is_available = true
        }).

-record(state, {
          keys    = [],
          servers = []
         }).

%%--------------------------------------------------------------------
%%
%% Gen Server API
%%
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Keys= registry:list_entriesDB(),
    {ok, #state{keys = Keys}}.

%%--------------------------------------------------------------------
%%
%% General API
%%
%%--------------------------------------------------------------------
get_servers() ->
    gen_server:call(?MODULE, get_servers).

got_ping(PublicKey) ->
    gen_server:cast(?MODULE, {ping, PublicKey}).

%%--------------------------------------------------------------------
%%
%% Call Handling
%%
%%--------------------------------------------------------------------
handle_call(get_servers, _From, #state{servers = Servers} = State) ->
    Reply = {servers, Servers},
    {reply, Reply, State}.

handle_cast({ping, PublicKey}, #state{servers = Servers} = State) ->
    io:format("State is ~p~nPublicKey is ~p~n", [State, PublicKey]),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("not handling message...~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
