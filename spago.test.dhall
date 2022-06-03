let conf = ./spago.dhall

in conf // {
    sources = conf.sources # [ "test/**/*.purs" ],
    dependencies = conf.dependencies #
      [ "react-testing-library"
      , "react-basic-dom"
      , "spec"
      , "spec-discovery"
      , "foreign-object"
      , "web-dom"
      , "arrays"
      , "strings"
      , "debug"
      , "tailrec"
      ]
}
