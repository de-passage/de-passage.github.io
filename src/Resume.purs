module Resume (component) where

import Content.Skills as S
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Languages as L
import Personal (personalInformation)
import Prelude (Unit, absurd, const, unit, ($))
import Projects as Projects
import Work as W

type State
  = Unit

data Action
  = Unit

type ChildSlots
  = ( projects :: Projects.Slot Unit
    )

_projects :: SProxy "projects"
_projects = SProxy

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    personal = HH.div [ HP.classes [ BS.colMd4, BS.colSm5, BS.col12 ] ] [ personalInformation, L.languages ]

    skill =
      HH.div [ HP.classes [ BS.colMd8, BS.colSm7, BS.col12 ] ]
        [ W.workExperience
        , S.technicalSkills
        , HH.slot _projects unit Projects.component unit absurd
        ]
  in
    HH.div [ HP.class_ BS.containerFluid ]
      [ HH.div [ HP.class_ BS.row ]
          [ personal
          , skill
          ]
      ]