let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220531/packages.dhall
        sha256:278d3608439187e51136251ebf12fabda62d41ceb4bec9769312a08b56f853e3

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
