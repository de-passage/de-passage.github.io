module Projects where

import Data.Map
import Affjax (Error, printError)
import Assets as A
import Category (categoryHidden)
import Data.Argonaut (Json, jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (snoc)
import Data.Either (Either, either)
import Data.Maybe (fromMaybe, maybe, Maybe)
import Data.Tuple.Nested ((/\))
import Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (ListItem, listGroup, listGroupC, listItem_)
import Prelude (bind, map, pure, ($), (>>=))

type Project
  = { html_url :: String
    , name :: String
    , description :: Maybe String
    , language :: Maybe String
    , homepage :: Maybe String
    }

data LoadStatus
  = Loading
  | LoadingError Error
  | Loaded String

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
    iconDiv = do
      lang <- pro.language
      iconName <- lookup lang icons
      pure $ HH.div [ HP.class_ BS.col1 ] [ A.iconS iconName [ HP.class_ (HH.ClassName "projectIcon") ] ]

    projNameDiv =
      HH.div [ HP.classes [ BS.col10, BS.colLg ] ]
        [ HH.a [ HP.href pro.html_url, HP.target "_blank" ] [ HH.text pro.name ] ]

    titleDiv =
      maybe
        [ projNameDiv, HH.div [ HP.classes [ BS.col1 ] ] [] ]
        (\icon -> [ icon, projNameDiv ])
        iconDiv
  in
    listItem_
      [ HH.div
          [ HP.class_ BS.row ]
          ( titleDiv
              `snoc`
                HH.div [ HP.classes [ BS.col12, BS.colLg ] ] [ F.para $ fromMaybe "" pro.description ]
          )
      ]

mkErrorMsg :: forall x j. String -> Array (ListItem x j)
mkErrorMsg err = [ listItem_ [ HH.text err ] ]

projects :: forall w i. LoadStatus -> HH.HTML w i
projects Loading =
  categoryHidden "projects" "Projects"
    [ HH.div
        [ HP.class_ BS.spinnerBorder, ARIA.role "status" ]
        [ HH.span [ HP.class_ BS.srOnly ] [] ]
    ]

projects (LoadingError s) =
  categoryHidden "projects" "Projects"
    [ listGroup $ mkErrorMsg (printError s) ]

projects (Loaded s) =
  let
    c = jsonParser s >>= jsonToProject

    container = listGroupC [ BS.textLeft, (HH.ClassName "project-list") ]

    content = container (either mkErrorMsg (map mkProject) c)
  in
    categoryHidden "projects" "Projects" [ content ]
