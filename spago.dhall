{ name = "mermaid"
, dependencies =
  [ "effect"
  , "free"
  , "maybe"
  , "prelude"
  , "st"
  , "tailrec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
