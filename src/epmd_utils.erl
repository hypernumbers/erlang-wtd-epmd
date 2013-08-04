%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2013 by gordonguthrie@backawinner.gg

-module(epmd_utils).

-export([
         recursive_copy/2,
         get_www_root/0
        ]).

%% Recursively copy directories
-spec recursive_copy(list(), list()) -> ok.
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    [ok = rec_copy1(From, To, X) || X <- Files],
    ok.

% ignore hidden
rec_copy1(_From, _To, [$. | _T]) ->
    ok;
rec_copy1(From, To, File) ->

    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),

    case filelib:is_dir(NewFrom) of

        true  ->
            ok = filelib:ensure_dir(NewTo),
            recursive_copy(NewFrom, NewTo);

        false ->
            case filelib:is_file(NewFrom) of
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok
            end
    end.

get_www_root() ->
{ok, Root} = file:get_cwd(),
    Root ++ "/" ++
        filename:dirname(code:where_is_file("wtdepmd.app")) ++
        "/../var/docroot/".
