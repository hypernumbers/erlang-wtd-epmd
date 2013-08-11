%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Holds the registry of available and verified
%%%            Erlang WTD nodes
%%%
%%% @end
%%% Created :  6 Aug 2013 by gordonguthrie@backawinner.gg

-module(registry).

-export([
         lookup/1,
         add_node/1,
         add_nodeDB/2,
         verifyDB/1,
         list_entriesDB/0
         ]).

-include("registry.hrl").

lookup(PublicKey) when is_list(PublicKey) ->
    Table = registry,
    ReadFn = fun() ->
                     case mnesia:read(Table, PublicKey, read) of
                         []  -> {error, no_key};
                         [R] -> {ok, R}
                     end
             end,
    mnesia:activity(transaction, ReadFn).

add_node(PublicKey) when is_list(PublicKey) ->
    PrivateKey = mochihex:to_hex(binary_to_list(crypto:strong_rand_bytes(16))),
    ok = add_nodeDB(PublicKey, PrivateKey),
    {ok, PrivateKey}.

add_nodeDB(PublicKey, PrivateKey) when is_list(PublicKey)  andalso
                                       is_list(PrivateKey) ->
    Table = registry,
    Record = #registry{public_key = PublicKey,
                       private_key = PrivateKey},
    WriteFn = fun() ->
                       ok = mnesia:write(Table, Record, write)
               end,
    mnesia:activity(transaction, WriteFn).

verifyDB(PublicKey) when is_list(PublicKey) ->
    Table = registry,
    UpdateFn =
        fun() ->
                case mnesia:read(Table, PublicKey, write) of
                    [#registry{} = R] ->
                        NewR = R#registry{verified = true},
                        mnesia:write(Table, NewR, write);
                    [] ->
                        {error, not_found}
                end
        end,
    mnesia:activity(transaction, UpdateFn).

list_entriesDB() ->
    Table = registry,
    FilterFn = fun() ->
                       Spec = #registry{_='_'},
                       mnesia:match_object(Table, Spec, read)
               end,
    mnesia:activity(transaction, FilterFn).

