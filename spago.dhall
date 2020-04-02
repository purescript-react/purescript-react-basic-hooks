{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-basic-hooks"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "indexed-monad"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
