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
         add_node/3
         ]).

add_node(Name, PublicKey) ->
    PrivateKey = mochihex:to_hex(binary_to_list(crypto:strong_rand_bytes(16))),
    ok = add_node(Name, PublicKey, PrivateKey),
    {ok, PrivateKey}.

add_node(Name, PublicKey, PrivateKey) ->
    io:format("fix up...~n"),
    ok.
