{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-smash"
, dependencies = [ "console", "effect", "psci-support", "prelude", "day", "record", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
