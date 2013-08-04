-type crasher() :: atom().

-record(bootstrap_test,
        {
          integer = 0       :: int(), % ergo
          float   = 1.0     :: float(),
          string  = "howdy" :: string(),
          atom    = bish    :: bish | bash | bosh
        }).
