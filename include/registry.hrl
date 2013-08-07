%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2013 by gordonguthrie@backawinner.gg

-record(registry,
        {
          name,
          verified,
          is_available,
          public_key,
          private_key
        }).
