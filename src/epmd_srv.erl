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
         got_ping/2
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

-record(state, {
          keys    = [],
          servers = dict:new()
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

got_ping({PublicKey, Name}, Missions) ->
    gen_server:cast(?MODULE, {ping, {{PublicKey, Name}, Missions}}).

%%--------------------------------------------------------------------
%%
%% Call Handling
%%
%%--------------------------------------------------------------------
handle_call(get_servers, _From, #state{servers = Servers} = State) ->
    Reply = {servers, dict:to_list(Servers)},
    {reply, Reply, State}.

handle_cast({ping, {{_PublicKey, _Name} = S, Missions}}, State) ->
    #state{servers = Svs} = State,
    NewServers = dict:store(S, Missions, Svs),
    {noreply, State#state{servers = NewServers}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
