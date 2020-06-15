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
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Format (para)
import Format as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (ListItem, listGroupC, listItem_)
import Prelude (Unit, Void, bind, const, map, pure, (#), ($), (==), (>=>), (>>>), (||))

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
  = LoadStatus

type Slot
  = H.Slot Query Message

data Action
  = Init
  | Toggle String

type ChildSlots
  = ()

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

component :: forall i m. MonadAff m => H.Component HH.HTML Query i Void m
component =
  H.mkComponent
    { initialState: const Loading
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  categoryHidden "projects" "Projects"
    [ subcategory "featured" "Featured"
        [ HH.div [ HP.classes [ BS.textLeft, BS.m2 ] ]
            [ HH.h2_ [ HH.a [ HP.href "https://sylvainleclercq.com/conduit.purs", HP.target "_blank" ] [ HH.text "Conduit" ] ]
            , para
                """Conduit is a clone of the popular blogging website Medium, intended to showcase how a real world
              website is implemented using various backend and frontend frameworks. This particular front-end implementation is my
              own and uses Purescript and Halogen."""
            , HH.p_
                [ HH.text
                    """While I am working on a backend implementation, it pulls its content from a public testing API, 
                  containing mostly nonsense. Feel free to """
                , HH.a [ HP.href "https://sylvainleclercq.com/conduit.purs/#/register", HP.target "_blank" ] [ HH.text "register" ]
                , HH.text " and play with the app."
                ]
            , HH.p_
                [ HH.text "The code is available on "
                , HH.a [ HP.href "https://github.com/de-passage/conduit.purs", HP.target "_blank" ] [ HH.text " my Github." ]
                ]
            , HH.div [ HP.class_ (H.ClassName "iframe-wrapper") ]
                [ HH.iframe [ HP.src "https://sylvainleclercq.com/conduit.purs" ]
                ]
            ]
        ]
    , subcategoryHidden "github" "Github Repositories" [ renderProjects state ]
    ]
  where
  renderProjects :: State -> H.ComponentHTML Action ChildSlots m
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
    H.put (either LoadingError (_.body >>> parse >>> loaded) response)
  Toggle lang -> H.modify_ (adaptFilter lang)
  where
  adaptFilter :: String -> LoadStatus -> LoadStatus
  adaptFilter lang (Loaded s) = Loaded $ s { languageFilter = if s.languageFilter == lang then "" else lang }

  adaptFilter _ s = s

parse :: String -> Either String (Array Project)
parse = jsonParser >=> jsonToProject

loaded :: Either String (Array Project) -> LoadStatus
loaded content = Loaded { projects: content, languageFilter: "" }
