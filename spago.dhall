{ name = "mermaid"
, dependencies =
  [ "bolson"
  , "effect"
  , "functions"
  , "hyrule"
  , "maybe"
  , "prelude"
  , "profunctor"
  , "st"
  , "tailrec"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
