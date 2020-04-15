{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "halogen-bootstrap4"
  , "halogen-css"
  , "psci-support"
  , "web-dom"
  , "web-html"
  , "halogen-svg"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
