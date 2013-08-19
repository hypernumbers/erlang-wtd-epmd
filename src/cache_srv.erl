%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       this is the cache server that the Rx calls are routed
%%%            to
%%%
%%% @end
%%% Created :  8 Aug 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(cache_srv).

-behaviour(gen_server).

-define(TIMEOUT, 5000).

%% API
-export([
         start_link/0
        ]).

%% General API
-export([
         rx/2,
         tx/2
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
-define(TICK,   5000).   % milli seconds
-define(EXPIRE, 700000). % micro seconds

-record(state, {
          servers = dict:new(),
          timeout = ?TIMEOUT
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
rx(Server, Pid) ->
    gen_server:call(?MODULE, {rx, {Server, Pid}}).

tx(Server, Msg) ->
    gen_server:call(?MODULE, {tx, {Server, Msg}}).

%%--------------------------------------------------------------------
%%
%% Call Handling
%%
%%--------------------------------------------------------------------
handle_call({rx, {Server, Pid}}, _From, #state{servers = Servers} = State) ->
    NewServers = dict:store(Server, Pid, Servers),
    Reply = ?TIMEOUT,
    {reply, Reply, State#state{servers = NewServers}};
handle_call({tx, {Server, Data}}, _From, #state{servers = Servers} = State) ->
    io:format("In tx for ~p~n", [Data]),
    {Reply, NewS} = case dict:find(Server, Servers) of
                        error       -> {{error, not_connected}, Servers};
                        {ok, Value} -> Value ! Data,
                                       {{ok, ack}, dict:erase(Server, Servers)}
                    end,
    {reply, Reply, State#state{servers = NewS}}.

handle_cast(_Msg, State) ->
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
