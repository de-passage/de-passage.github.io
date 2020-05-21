module Projects (component, Slot, Message(..), Query, LoadStatus) where

import Affjax (Error, printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Assets as A
import Category (categoryHidden)
import Data.Argonaut (Json, jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (mapMaybe, nub, snoc, sort)
import Data.Const (Const)
import Data.Either (Either, either)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Format as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (ListItem, listGroupC, listItem_)
import Prelude (Unit, Void, bind, const, map, pure, (#), ($), (>>=), (>>>))

type Project
  = { html_url :: String
    , name :: String
    , description :: Maybe String
    , language :: Maybe String
    , homepage :: Maybe String
    }

type Message
  = Void

type Query
  = Const Void

data LoadStatus
  = Loading
  | LoadingError Error
  | Loaded String

type Slot
  = H.Slot Query Message

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

mkErrorMsg :: forall x j. String -> HH.HTML x j
mkErrorMsg err = HH.div [ HP.classes [ BS.alertDanger, BS.alert ] ] [ HH.text err ]

projects :: forall w i. LoadStatus -> HH.HTML w i
projects Loading =
  categoryHidden "projects" "Projects"
    [ HH.div
        [ HP.class_ BS.spinnerBorder, ARIA.role "status" ]
        [ HH.span [ HP.class_ BS.srOnly ] [] ]
    ]

projects (LoadingError s) =
  categoryHidden "projects" "Projects"
    [ mkErrorMsg (printError s) ]

projects (Loaded s) =
  let
    c = jsonParser s >>= jsonToProject

    content = either mkErrorMsg mkProjectList c
  in
    categoryHidden "projects" "Projects" [ content ]

mkProjectList :: forall w i. Array Project -> HH.HTML w i
mkProjectList prjkts =
  HH.div []
    [ HH.div [] (mapMaybe (_.language) prjkts # nub >>> sort >>> map mkLanguageButton)
    , listGroupC [ BS.textLeft, (HH.ClassName "project-list") ] (map mkProject prjkts)
    ]

mkLanguageButton :: forall w i. String -> HH.HTML w i
mkLanguageButton p = HH.button [ HP.classes [ BS.btn, BS.btnOutlineSecondary, BS.mr1 ] ] [ HH.text p ]

type State
  = LoadStatus

data Action
  = Init

type ChildSlots
  = ()

component :: forall i m. MonadAff m => H.Component HH.HTML Query i Void m
component =
  H.mkComponent
    { initialState: const Loading
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state = projects state

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction Init = do
  response <- H.liftAff $ AX.get AXRF.string "https://api.github.com/users/de-passage/repos?sort=updated"
  H.put (either LoadingError (_.body >>> Loaded) response)
