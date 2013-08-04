%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Makes boostrap forms from record definitions
%%%
%%% @end
%%% Created :  3 Aug 2013 by gordonguthrie@backawinner.gg

-module(bootstrap_make).

-export([
         test/0
        ]).

-export([
         make/1
        ]).

test() ->
    File = "/home/vagrant/erlang-wtd-epmd/test/bootstrap/bootstrap.hrl",
    make(File).

make(File) ->
    {ok, Tree} = epp:parse_file(File, [], []),
    Src = make_src(Tree, []),
    io:format("Src is ~p~n", [Src]),
    ok.

make_src([], A)                                   -> lists:reverse(A);
make_src([{attribute, _, record, Record} | T], A) -> make_src(T, [Record | A]);
make_src([H | T], A)                              -> io:format("H is ~p~n", [H]),
                                                     make_src(T, A).
