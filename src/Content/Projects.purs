module Projects where

import Data.Map

import Assets as A
import Category (categoryHidden)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (catMaybes)
import Data.Either (Either, either)
import Data.Maybe (fromMaybe, maybe, Maybe(..))
import Data.Tuple.Nested ((/\))
import Format as F
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (ListItem, listGroupC, listItem_)
import Prelude (map, (>>>), (>>=), flip, (#), ($), (<#>), (<$>))

type ProjectJSON
  = { html_url :: String
    , name :: String
    , description :: Maybe String
    , language :: Maybe String
    , languages_url :: Maybe String
    , homepage :: Maybe String
    }

type Project 
  = { project :: ProjectJSON
    , languages :: Array String
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
    , "Python" /\ A.pythonIcon
    , "C#" /\ A.csharpIcon
    , "Lua" /\ A.luaIcon
    ]

jsonToProject :: Json -> Either String (Array ProjectJSON)
jsonToProject = decodeJson

mkProject :: forall w i. Project -> ListItem w i
mkProject pro =
  let
    iconDivs :: Array String -> Array (HH.HTML w i)
    iconDivs l =
      (l <#> flip lookup icons)
        <#> (\language -> language >>= (\l' -> Just $ A.iconS l' [ HP.class_ (HH.ClassName "projectIcon") ]))
        # catMaybes
  in
    listItem_
      [ HH.div
          [ HP.class_ BS.row ]
          [ HH.div [ HP.classes [ BS.col ] ]
              (iconDivs pro.languages)
          , HH.div [ HP.classes [ BS.col ] ]
              [ HH.a [ HP.href pro.project.html_url ] [ HH.text pro.project.name ] ]
          , HH.div [ HP.classes [ BS.col ] ]
            [ F.para $ fromMaybe "" pro.project.description ]
          ]
      ]

projects :: forall w i. Maybe (Array Project) -> HH.HTML w i
projects Nothing =
  categoryHidden "projects" "Projects"
    [ HH.div [ HP.class_ BS.spinnerBorder, ARIA.role "status" ] [ HH.span [ HP.class_ BS.srOnly ] [] ] ]

projects (Just s) =
  let

    prjkts = map mkProject s
  in
    categoryHidden "projects" "Projects"
      [ listGroupC [ BS.textLeft ] prjkts
      ]
