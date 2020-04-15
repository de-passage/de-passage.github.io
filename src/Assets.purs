module Assets
  ( Icon
  , icon
  , iconList
  , iconList_
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
  , decorate
  ) where

import CSS (i)
import CSS as CSS
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements as HE
import Halogen.HTML.Properties as HP
import Prelude (($), map, (<>), (<<<), discard)

newtype Icon w i
  = Icon (HH.HTML w i)

derive instance newtypeIcon :: Newtype (Icon w i) _

-- kind unification fails. iconList :: forall r w i. Array (HP.IProp r i) -> Array (Icon w i) -> HH.HTML w i
iconList props icons = HH.div props $ map unwrap icons

decorate :: forall w i x j. (HH.HTML w i -> HH.HTML x j) -> Icon w i -> Icon x j
decorate f (Icon i) = (Icon $ f i)

icon :: forall w i. String -> Icon w i
icon name =
  let
    rem1 = CSS.em 1.0

    iconDimension = CSS.em 9.0

    style = do
      CSS.padding rem1 rem1 rem1 rem1
      CSS.height iconDimension
      CSS.width iconDimension
  in
    Icon
      $ HH.img
          [ HP.src $ "https://unpkg.com/simple-icons@latest/icons/" <> name <> ".svg"
          , HC.style style
          ]

iconList_ :: forall w i. Array (Icon w i) -> HH.HTML w i
iconList_ = iconList []

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
