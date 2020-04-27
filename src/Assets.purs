module Assets
  ( Icon
  , icon
  , iconS
  , purescriptIcon
  , elmIcon
  , cppIcon
  , haskellIcon
  , cIcon
  , luaIcon
  , javascriptIcon
  , csharpIcon
  , rubyIcon
  , pythonIcon
  , coffeescriptIcon
  , htmlIcon
  , cssIcon
  , rustIcon
  ) where

import CSS as CSS
import Data.Array (snoc)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Prelude (($), (<>), discard, bind, pure)

type Icon w i
  = Number -> HH.HTML w i

source name = HP.src $ "https://unpkg.com/simple-icons@latest/icons/" <> name <> ".svg"

icon :: forall w i. String -> Icon w i
icon name =
    do
      iconDimension <- CSS.em
      let em1 = CSS.em 1.0
      let
        style = do
          CSS.padding em1 em1 em1 em1
          CSS.height iconDimension
          CSS.width iconDimension
      pure $ HH.img
            [ source name
            , HC.style style
            ]

iconS name a = HH.img (a `snoc` (source name))

purescriptIcon :: String
purescriptIcon = "purescript"

elmIcon :: String
elmIcon = "elm"

cppIcon :: String
cppIcon = "cplusplus"

haskellIcon :: String
haskellIcon = "haskell"

luaIcon :: String
luaIcon = "lua"

cIcon :: String
cIcon = "c"

javascriptIcon :: String
javascriptIcon = "javascript"

csharpIcon :: String
csharpIcon = "csharp"

coffeescriptIcon :: String
coffeescriptIcon = "coffeescript"

rubyIcon :: String
rubyIcon = "ruby"

pythonIcon :: String
pythonIcon = "python"

htmlIcon :: String
htmlIcon = "html5"

cssIcon :: String
cssIcon = "css3"

rustIcon :: String
rustIcon = "rust"