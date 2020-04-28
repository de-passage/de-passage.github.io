module Resume (component) where

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Content.Skills as S
import Data.Argonaut (Json, jsonParser, toObject)
import Data.Either (either, hush)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Languages as L
import Personal (personalInformation)
import Prelude (Unit, const, ($), (>>>), bind, pure, map, (>>=), (#), (<$>), (<#>))
import Projects (ProjectJSON, jsonToProject)
import Projects as P
import Work as W

type State
  = Maybe (Array P.Project)

data Action
  = Init

type ChildSlots
  = ()

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    personal = HH.div [ HP.classes [ BS.colMd4, BS.colSm5, BS.col12 ] ] [ personalInformation, L.languages ]

    skill =
      HH.div [ HP.classes [ BS.colMd8, BS.colSm7, BS.col12 ] ]
        [ W.workExperience, S.technicalSkills, P.projects state ]
  in
    HH.div [ HP.class_ BS.containerFluid ]
      [ HH.div [ HP.class_ BS.row ]
          [ personal
          , skill
          ]
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction Init = do
  response <- H.liftAff $ AX.get AXRF.string "https://api.github.com/users/de-passage/repos?sort=updated"
  let
    marr = do
      r <- hush response
      values <- hush $ jsonParser r.body
      hush $ jsonToProject values

  projects <- getProjects marr
  H.put projects
  where
  getProjects :: Maybe (Array P.ProjectJSON) -> H.HalogenM State Action ChildSlots o m (Maybe (Array P.Project))
  getProjects ma = case ma of
                    Nothing -> pure Nothing 
                    Just a -> buildProjects a <#> Just

  buildProjects :: Array P.ProjectJSON -> H.HalogenM State Action ChildSlots o m (Array P.Project)
  buildProjects = traverse $ \proj ->
      case proj.languages_url of
        Nothing -> pure $ { project: proj, languages: [] }
        Just url ->  getLangs url <#> (\a -> { project: proj, languages: a })

  getLangs :: String -> H.HalogenM State Action ChildSlots o m (Array String)
  getLangs url = fetchLangs url <#> parseLangs

  fetchLangs :: String -> H.HalogenM State Action ChildSlots o m (Maybe (AX.Response String))
  fetchLangs url = H.liftAff (AX.get AXRF.string url <#> hush)

  parseLangs :: forall r. Maybe { body :: String | r } -> Array String
  parseLangs j = j >>= _.body >>> jsonParser >>> hush <#> jsonToLangs # fromMaybe []

  jsonToLangs :: Json -> Array String
  jsonToLangs json = toObject json <#> FO.keys # fromMaybe []