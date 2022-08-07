{ name = "mermaid"
, dependencies =
  [ "bolson"
  , "effect"
  , "free"
  , "hyrule"
  , "maybe"
  , "prelude"
  , "profunctor"
  , "st"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
