{ name = "mermaid"
, dependencies =
  [ "effect"
  , "free"
  , "maybe"
  , "mmorph"
  , "prelude"
  , "st"
  , "tailrec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
