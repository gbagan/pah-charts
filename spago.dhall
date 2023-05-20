{ name = "pha-charts"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "pha"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
