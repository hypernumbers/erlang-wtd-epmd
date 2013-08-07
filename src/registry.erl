%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Holds the registry of available and verified
%%%            Eralng WTD nodes
%%%
%%% @end
%%% Created :  6 Aug 2013 by gordonguthrie@backawinner.gg

-module(registry).

-export([
         add_node/2,
         add_nodeDB/3,
         verifyDB/1,
         set_availabilityDB/2,
         list_availableDB/0
         ]).

-include("registry.hrl").

add_node(Name, PublicKey) when is_list(Name) andalso is_list(PublicKey) ->
    PrivateKey = mochihex:to_hex(binary_to_list(crypto:strong_rand_bytes(16))),
    ok = add_nodeDB(Name, PublicKey, PrivateKey),
    {ok, PrivateKey}.

add_nodeDB(Name, PublicKey, PrivateKey) when is_list(Name)       andalso
                                             is_list(PublicKey)  andalso
                                             is_list(PrivateKey) ->
    Table = registry,
    Record = #registry{name = Name,
                       public_key = PublicKey,
                       private_key = PrivateKey},
    WriteFn = fun() ->
                       ok = mnesia:write(Table, Record, write)
               end,
    mnesia:activity(transaction, WriteFn).

verifyDB(PublicKey) when is_list(PublicKey) ->
    Table = registry,
    UpdateFn =
        fun() ->
                case mnesia:index_read(Table, PublicKey, public_key) of
                    [#registry{} = R] ->
                        NewR = R#registry{verified = true},
                        mnesia:write(Table, NewR, write);
                    [] ->
                        {error, not_found}
                end
        end,
    mnesia:activity(transaction, UpdateFn).

set_availabilityDB(Name, Availability) when is_list(Name)            andalso
                                            is_boolean(Availability) ->
    Table = registry,
    UpdateFn =
        fun() ->
                case mnesia:read(Table, Name, write) of
                    [#registry{verified = true} = R] ->
                        NewR = R#registry{is_available = Availability},
                        mnesia:write(Table, NewR, write);
                    [#registry{verified = false}] ->
                        {error, not_verified};
                    [] ->
                        {error, not_found}
                end
        end,
    mnesia:activity(transaction, UpdateFn).

list_availableDB() ->
    Table = registry,
    FilterFn = fun() ->
                       Spec = #registry{_='_', is_available = true},
                       mnesia:match_object(Table, Spec, read)
               end,
    mnesia:activity(transaction, FilterFn).

