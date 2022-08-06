{ name = "mermaid"
, dependencies =
  [ "effect"
  , "free"
  , "maybe"
  , "prelude"
  , "st"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
