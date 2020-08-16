let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200724/packages.dhall sha256:bb941d30820a49345a0e88937094d2b9983d939c9fd3a46969b85ce44953d7d9

let overrides = {=}

let additions = 
    { day =
       { dependencies =
           [ "exists"
           , "pairing"
           ]
       , repo =
           "https://github.com/paf31/purescript-day.git"
       , version =
           "master"
       }
    }
in  upstream // overrides // additions
