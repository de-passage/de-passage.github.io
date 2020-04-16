module Assets
  ( Icon
  , icon
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
  ) where

import CSS as CSS
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Prelude (($), (<>), discard, bind, pure)

type Icon w i
  = Number -> HH.HTML w i

icon :: forall w i. String -> Icon w i
icon name =
    do
      iconDimension <- CSS.em
      let rem1 = CSS.em 1.0
      let
        style = do
          CSS.padding rem1 rem1 rem1 rem1
          CSS.height iconDimension
          CSS.width iconDimension
      pure $ HH.img
            [ HP.src $ "https://unpkg.com/simple-icons@latest/icons/" <> name <> ".svg"
            , HC.style style
            ]

d :: forall w i. String -> HH.IProp ( d :: String | w ) i
d = HP.attr (HH.AttrName "d")

purescriptIcon :: forall w i. Icon w i
purescriptIcon = icon "purescript"

elmIcon :: forall w i. Icon w i
elmIcon = icon "elm"

cppIcon :: forall w i. Icon w i
cppIcon = icon "cplusplus"

haskellIcon :: forall w i. Icon w i
haskellIcon = icon "haskell"

luaIcon :: forall w i. Icon w i
luaIcon = icon "lua"

cIcon :: forall w i. Icon w i
cIcon = icon "c"

javascriptIcon :: forall w i. Icon w i
javascriptIcon = icon "javascript"

csharpIcon :: forall w i. Icon w i
csharpIcon = icon "csharp"

coffeescriptIcon :: forall w i. Icon w i
coffeescriptIcon = icon "coffeescript"

rubyIcon :: forall w i. Icon w i
rubyIcon = icon "ruby"

pythonIcon :: forall w i. Icon w i
pythonIcon = icon "python"

htmlIcon :: forall w i. Icon w i
htmlIcon = icon "html5"

cssIcon :: forall w i. Icon w i
cssIcon = icon "css3"
