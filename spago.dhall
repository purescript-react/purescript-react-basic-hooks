{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-basic-hooks"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "functions"
  , "indexed-monad"
  , "integers"
  , "maybe"
  , "newtype"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "react-basic"
  , "refs"
  , "tuples"
  , "type-equality"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "Apache-2.0"
, repository = "https://github.com/megamaddu/purescript-react-basic-hooks"
}
