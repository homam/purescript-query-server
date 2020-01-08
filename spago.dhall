{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "QueryDSL"
    , "aff"
    , "aff-promise"
    , "argonaut"
    , "argonaut-codecs"
    , "argonaut-core"
    , "argonaut-generic"
    , "console"
    , "effect"
    , "express"
    , "foreign"
    , "generics-rep"
    , "js-date"
    , "js-timers"
    , "node-postgres"
    , "node-process"
    , "numbers"
    , "ordered-collections"
    , "parsing"
    , "psci-support"
    , "simple-json"
    , "spec"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
