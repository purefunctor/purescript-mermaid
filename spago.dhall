{ name = "mermaid"
, dependencies =
  [ "aff", "control", "effect", "free", "prelude", "refs", "spec", "st" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
