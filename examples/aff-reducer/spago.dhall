{ name = "aff-reducer-example"
, dependencies =
    [ "affjax"
    , "console"
    , "prelude"
    , "effect"
    , "psci-support"
    , "react-basic"
    , "indexed-monad"
    , "unsafe-reference"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "../../src/**/*.purs", "test/**/*.purs" ]
}
