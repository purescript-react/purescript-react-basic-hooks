{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-basic-hooks"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "indexed-monad"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "react-basic"
  , "type-equality"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "Apache-2.0"
, repository = "https://github.com/spicydonuts/purescript-react-basic-hooks"
}
