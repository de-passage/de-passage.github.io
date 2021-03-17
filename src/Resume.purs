module Resume (component) where

import Content.Skills as S
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Languages as L
import Personal as P
import Prelude (Unit, absurd, identity, unit, ($))
import Projects as Projects
import State (State, Input, Action(..))
import Work as W

type ChildSlots
  = P.ChildSlots
      ( L.ChildSlots
          ( projects :: Projects.Slot Unit
          )
      )

_projects :: SProxy "projects"
_projects = SProxy

component :: forall q o m. MonadAff m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction action = case action of
    LanguageChanged lang -> H.modify_ (_ { language = lang })

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    personal = HH.div [ HP.classes [ BS.colMd4, BS.colSm5, BS.col12 ] ] [ P.personalInformation state, L.languages state ]

    skill =
      HH.div [ HP.classes [ BS.colMd8, BS.colSm7, BS.col12 ] ]
        [ W.workExperience
        , S.technicalSkills state
        , HH.slot _projects unit Projects.component state absurd
        ]
  in
    HH.div [ HP.class_ BS.containerFluid ]
      [ HH.div [ HP.class_ BS.row ]
          [ personal
          , skill
          ]
      ]
