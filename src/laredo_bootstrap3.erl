%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This is a help module for generating bootstrap websites
%%%
%%% @end
%%% Created :  2 Aug 2013 by gordonguthrie@backawinner.gg

-module(laredo_bootstrap3).

-export([
         well/1,
         hero/1,
         hero/2,
         form_field/3,
         form_checkbox/1,
         form/3
         %% table_from_lists/1,
         %% record/1
        ]).

-import(html, [
               header/1,
               header/2,
               header/3,

               dv/1,
               dv/2,
               dv/3,

               p/1,
               p/2,
               p/3,

               h1/1,
               h1/2,
               h1/3,

               h2/1,
               h2/2,
               h2/3,

               h3/1,
               h3/2,
               h3/3,

               h4/1,
               h4/2,
               h4/3,

               h5/1,
               h5/2,
               h5/3,

               h6/1,
               h6/2,
               h6/3,

               a/2,
               a/3,
               a/4,
               a/5
              ]).

-define(MEGA, 1000000000000).
-define(SEC,  1000000).

hero(Headline) ->
    hero(Headline, []).

hero(Headline, Body) ->
    H1 = h1(Headline),
    P  = p(Body),
    lists:flatten(header(dv([H1, P], "", "hero-unit"))).

form_field(Label, Placeholder, Help) ->
    "<label>" ++ Label ++ "</label>" ++
        "<input type='text' placeholder='" ++ Placeholder ++ "'>" ++
        "<span class='help-block'>" ++ Help ++ "</span>".

form_checkbox(Text) ->
    "<label class='checkbox'>" ++
        "<input type='checkbox'>" ++ Text ++
        "</label>".

form(Legend, Fields, ButtonTxt) ->
    "<form id='" ++ get_id("Form") ++ "'>" ++
        "<fieldset>" ++
        "<legend>" ++ Legend ++ "</legend>" ++
        lists:flatten(Fields) ++
        "<button type='button' " ++
        "class='btn laredo-submit'>" ++
        ButtonTxt ++
        "</button>" ++
        "</fieldset>" ++
        "</form>".

well(HTML) ->
    "<div class='well'>" ++ lists:flatten(HTML) ++ "</div>".

get_id(Prefix) when is_list(Prefix) ->
    Prefix ++ integer_to_list(timestamp()).

timestamp() ->
    {Mega, Sec, Micro} = now(),
    ?MEGA * Mega + ?SEC * Sec + Micro.
