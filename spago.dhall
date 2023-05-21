{ name = "pha-charts"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "pha"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
