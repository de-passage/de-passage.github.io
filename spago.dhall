{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-bootstrap4"
  , "halogen-css"
  , "halogen-svg"
  , "parsing"
  , "psci-support"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
