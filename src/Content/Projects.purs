module Projects (component, Slot, Message(..), Query, LoadStatus) where

import Affjax (Error, printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Assets as A
import Category (categoryHidden, subcategory, subcategoryHidden)
import Data.Argonaut (Json, jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (filter, mapMaybe, nub, snoc, sort)
import Data.Const (Const)
import Data.Either (Either, either)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Format as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (ListItem, listGroupC, listItem_)
import Marked as M
import Prelude (Unit, Void, absurd, bind, const, map, pure, unit, (#), ($), (==), (>=>), (>>>), (||))
import State as S

projectsL = S.localize "projects" :: S.Localizer

reposL = S.localize "github-repos" :: S.Localizer

featuredL = S.localize "featured" :: S.Localizer

conduitL = S.localize conduitS :: S.Localizer

conduitS = "conduit-description" :: String

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
  | Loaded
    { projects :: Either String (Array Project)
    , languageFilter :: String
    }

type State
  = { status :: LoadStatus
    , globalState :: S.State
    }

type Input
  = S.State

type Slot
  = H.Slot Query Message

data Action
  = Init
  | Toggle String

type ChildSlots
  = ( conduitDescription :: M.Slot Unit )

_conduitDescription = SProxy :: SProxy "conduitDescription"

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

mkProjectList :: forall w. String -> Array Project -> HH.HTML w Action
mkProjectList langs prjkts =
  let
    filtr p = langs == "" || maybe false (_ == langs) p.language
  in
    HH.div []
      [ HH.div [ HP.class_ BS.my2 ] (mapMaybe (_.language) prjkts # nub >>> sort >>> map (mkLanguageButton langs))
      , listGroupC [ BS.textLeft, (HH.ClassName "project-list") ] (filter filtr prjkts # map mkProject)
      ]

mkLanguageButton :: forall w. String -> String -> HH.HTML w Action
mkLanguageButton f p =
  let
    style = if p == f then BS.btnSecondary else BS.btnOutlineSecondary
  in
    HH.button
      [ HE.onClick (const (Just $ Toggle p))
      , HP.classes [ BS.btn, BS.mr1, style ]
      ]
      [ HH.text p ]

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
  where
  initialState globalState = { globalState, status: Loading }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state@{ globalState } =
  categoryHidden "projects" (projectsL globalState)
    [ subcategory "featured" (featuredL globalState)
        [ HH.div [ HP.classes [ BS.textLeft, BS.m2 ] ]
            [ HH.slot _conduitDescription unit M.component { text: conduitL globalState, id: conduitS } absurd
            , HH.div [ HP.class_ (H.ClassName "iframe-wrapper") ]
                [ HH.iframe [ HP.src "https://sylvainleclercq.com/conduit.purs" ]
                ]
            ]
        ]
    , subcategoryHidden "github" (reposL globalState) [ renderProjects state.status ]
    ]
  where
  renderProjects :: LoadStatus -> H.ComponentHTML Action ChildSlots m
  renderProjects Loading =
    HH.div
      [ HP.class_ BS.spinnerBorder, ARIA.role "status" ]
      [ HH.span [ HP.class_ BS.srOnly ] [] ]

  renderProjects (LoadingError s) = mkErrorMsg (printError s)

  renderProjects (Loaded s) = either mkErrorMsg (mkProjectList s.languageFilter) s.projects

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    response <- H.liftAff $ AX.get AXRF.string "https://api.github.com/users/de-passage/repos?sort=updated"
    H.modify_ _ { status = (either LoadingError (_.body >>> parse >>> loaded) response) }
  Toggle lang -> H.modify_ (\s -> s { status = adaptFilter lang s.status })
  where
  adaptFilter :: String -> LoadStatus -> LoadStatus
  adaptFilter lang (Loaded s) = Loaded $ s { languageFilter = if s.languageFilter == lang then "" else lang }

  adaptFilter _ s = s

parse :: String -> Either String (Array Project)
parse = jsonParser >=> jsonToProject

loaded :: Either String (Array Project) -> LoadStatus
loaded content = Loaded { projects: content, languageFilter: "" }
