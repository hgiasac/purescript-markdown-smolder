{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "markdown-smolder"
, dependencies = 
  [ "console"
  , "effect"
  , "ordered-collections"
  , "psci-support"
  , "markdown"
  , "smolder" 
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
