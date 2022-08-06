{ name = "mermaid"
, dependencies =
  [ "effect"
  , "free"
  , "maybe"
  , "partial"
  , "prelude"
  , "st"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
