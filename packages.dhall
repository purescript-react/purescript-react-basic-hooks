let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220523/packages.dhall
        sha256:985f90fa68fd8b43b14c777d6ec2c161c4dd9009563b6f51685a54e4a26bf8ff

in  upstream
  with react-testing-library =
    { dependencies =
      [ "aff"
      , "aff-promise"
      , "control"
      , "effect"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "functions"
      , "identity"
      , "maybe"
      , "prelude"
      , "react-basic"
      , "spec"
      , "strings"
      , "transformers"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      ]
    , repo =
        "https://github.com/i-am-the-slime/purescript-react-testing-library"
    , version = "v4.0.1"
    }
  with react-basic-dom =
    { dependencies =
      [ "effect"
      , "foldable-traversable"
      , "foreign-object"
      , "maybe"
      , "nullable"
      , "prelude"
      , "react-basic"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-file"
      , "web-html"
      ]
    , repo = "https://github.com/Zelenya7/purescript-react-basic-dom"
    , version = "purescript-0.15-spago"
    }
