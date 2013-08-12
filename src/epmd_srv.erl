%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       this server supports the epmd service
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
         tick/0,
         get_servers/0,
         got_ping/2
        ]).

-include("epmd_srv.hrl").

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
-define(TICK,   5000).   % milli seconds
-define(EXPIRE, 700000). % micro seconds

-record(state, {
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
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%%
%% General API
%%
%%--------------------------------------------------------------------
tick() ->
    gen_server:cast(?MODULE, tick).

get_servers() ->
    gen_server:call(?MODULE, get_servers).

got_ping({PublicKey, Name}, Missions) ->
    gen_server:call(?MODULE, {ping, {{PublicKey, Name}, Missions}}).

%%--------------------------------------------------------------------
%%
%% Call Handling
%%
%%--------------------------------------------------------------------
handle_call(get_servers, _From, #state{servers = Servers} = State) ->
    Reply = {servers, Servers},
    {reply, Reply, State};
handle_call({ping, {{_PublicKey, _Name} = S, Missions}}, _From, State) ->
    #state{servers = Svs} = State,
    Time = epmd_utils:timestamp(),
    NewServers = lists:keystore(S, 1, Svs, {S, {Missions, Time}}),
    Reply = NewServers,
    {reply, Reply, State#state{servers = NewServers}}.

handle_cast(tick, #state{servers = Svs} = State) ->
    Now = epmd_utils:timestamp(),
    FilterFn = fun({_, {_Missions, Time}}) ->
                       if
                           (Now - Time) >  ?EXPIRE -> false;
                           (Now - Time) =< ?EXPIRE -> true
                       end
               end,
    NewSvs = lists:filtermap(FilterFn, Svs),
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    {noreply, State#state{servers = NewSvs}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
