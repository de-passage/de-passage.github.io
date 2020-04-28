module Projects where

import Data.Map

import Assets as A
import Category (categoryHidden)
import Data.Argonaut (Json, jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either, either)
import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.Tuple.Nested ((/\))
import Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (ListItem, listGroupC, listItem_)
import Prelude (map, (>>>), (>>=), flip, (#), ($))

type Project
  = { html_url :: String
    , name :: String
    , description :: Maybe String
    , language :: Maybe String
    , homepage :: Maybe String
    }

icons :: Map String String
icons =
  fromFoldable
    [ "C++" /\ A.cppIcon
    , "PureScript" /\ A.purescriptIcon
    , "CoffeeScript" /\ A.coffeescriptIcon
    , "C" /\ A.cIcon
    , "Haskell" /\ A.haskellIcon
    , "Ruby" /\ A.rubyIcon
    , "JavaScript" /\ A.javascriptIcon
    , "Rust" /\ A.rustIcon
    , "Elm" /\ A.elmIcon
    , "HTML" /\ A.htmlIcon
    , "CSS" /\ A.cssIcon
    ]

jsonToProject :: Json -> Either String (Array Project)
jsonToProject = decodeJson

mkProject :: forall w i. Project -> ListItem w i
mkProject pro =
  let
    iconDiv =
      pro.language
        >>= flip lookup icons
        # maybe (HH.text "") (\language -> A.iconS language [ HP.class_ (HH.ClassName "projectIcon") ])
  in
    listItem_
      [ HH.div
          [ HP.class_ BS.row ]
          [ HH.div [ HP.class_ BS.col1 ]
              [ iconDiv ]
          , HH.div [ HP.class_ BS.col ]
              [ HH.a [ HP.href pro.html_url ] [ HH.text pro.name ] ]
          , HH.div [ HP.class_ BS.col ]
            [ F.para $ fromMaybe "" pro.description ]
          ]
      ]

projects :: forall w i. Maybe String -> HH.HTML w i
projects Nothing =
  categoryHidden "projects" "Projects"
    [ HH.div [ HP.class_ BS.spinnerBorder, ARIA.role "status" ] [ HH.span [ HP.class_ BS.srOnly ] [] ] ]

projects (Just s) =
  let
    parsed = jsonParser s

    prjkts = either mkErrorMsg decodeThenMk parsed
  in
    categoryHidden "projects" "Projects"
      [ listGroupC [ BS.textLeft ] prjkts
      ]
  where
  decodeThenMk :: forall x j. Json -> Array (ListItem x j)
  decodeThenMk = jsonToProject >>> either mkErrorMsg (map mkProject)

  mkErrorMsg :: forall x j. String -> Array (ListItem x j)
  mkErrorMsg err = [ listItem_ [ HH.text err ] ]
