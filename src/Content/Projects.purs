module Projects where

import Category (categoryHidden)
import Data.Argonaut (Json, jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Lists (ListItem, listItem_, listGroup)
import Prelude (map, (>>>))

type Project
  = { html_url :: String
    , name :: String
    , description :: Maybe String
    , language :: Maybe String
    , homepage :: Maybe String
    }

jsonToProject :: Json -> Either String (Array Project)
jsonToProject = decodeJson

mkProject :: forall w i. Project -> ListItem w i
mkProject pro = listItem_ [ HH.text pro.html_url ]

projects :: forall w i. Maybe String -> HH.HTML w i
projects Nothing = categoryHidden "projects" "Projects" []

projects (Just s) =
  let
    parsed = jsonParser s

    prjkts = either (mkErrorMsg "Parsing") decodeThenMk parsed
  in
    categoryHidden "projects" "Projects"
      [ listGroup prjkts
      ]
  where
  decodeThenMk :: forall x j. Json -> Array (ListItem x j)
  decodeThenMk = jsonToProject >>> either (mkErrorMsg "Decoding") (map mkProject)

  mkErrorMsg :: forall x j. String -> String -> Array (ListItem x j)
  mkErrorMsg pref err = [ listItem_ [HH.text pref], listItem_ [ HH.text err ] ]
