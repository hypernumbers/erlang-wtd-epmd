%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This script compiles coffeescript to javascript
%%%
%%% @end
%%% Created :  3 Aug 2013 by gordonguthrie@backawinner.gg

-module(make_coffee).

-export([
         make_coffee/2
        ]).

-define(JSDIR, "var/docroot/_assets/laredo/js/").
-define(COFFEEDIR, "priv/coffeeroot/").

make_coffee(_A, _B) ->
    case has_coffeescript() of
        true  -> ok = clear_old_js(),
                 ok = filelib:ensure_dir(?JSDIR),
                 ok = brew(),
                 ok;
        false -> ok
    end.

brew() ->
    Files = filelib:wildcard("*.coffee", ?COFFEEDIR),
    CMD = "coffee -m -o " ++ ?JSDIR ++ " -c " ++ ?COFFEEDIR,
    [os:cmd(CMD ++ X) || X <- Files],
    ok.

clear_old_js() ->
    case file:list_dir(?JSDIR) of
        {error,   _} -> ok; % directory doesn't exist, that's ok
        {ok, Files}  -> [ok = file:delete(?JSDIR ++ X) || X <- Files],
                        ok
    end.

has_coffeescript() ->
    case file:list_dir(?COFFEEDIR) of
        {error, _} -> false;
        {ok, _   } -> true
    end.

