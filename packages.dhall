let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220522/packages.dhall
        sha256:43895efaec7af246b60b59cfbf451cd9d3d84a5327de8c0945e2de5c9fd2fcf2

in upstream
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
    , version = "v4.0.0"
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
    , repo =
        "https://github.com/Zelenya7/purescript-react-basic-dom"
    , version = "purescript-0.15-spago"
    }