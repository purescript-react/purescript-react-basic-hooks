let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220527/packages.dhall
        sha256:15dd8041480502850e4043ea2977ed22d6ab3fc24d565211acde6f8c5152a799

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
