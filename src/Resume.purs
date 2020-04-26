module Resume (component) where

import CSS (CSS)
import CSS as CSS
import Content.Skills as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Languages as L
import Personal (personalInformation)
import Prelude (Unit, Void, const, unit, pure)
import Projects (projects)
import Work as W

type State
  = Unit

type Action
  = Void

type ChildSlots
  = ()

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render _ =
  let
    personal = HH.div [ HP.classes [ BS.colMd4, BS.colSm5, BS.col12 ] ] [ personalInformation, L.languages ]

    skill =
      HH.div [ HP.classes [ BS.colMd8, BS.colSm7, BS.col12 ] ]
        [ W.workExperience, S.technicalSkills, projects ]
  in
    HH.div [ HP.class_ BS.containerFluid ]
      [ HH.div [ HP.class_ BS.row ]
          [ personal
          , skill
          ]
      ]

handleAction :: forall o m. Unit -> H.HalogenM State Action ChildSlots o m Unit
handleAction _ = pure unit

globalStyle :: CSS
globalStyle = do CSS.backgroundColor (CSS.rgb 240 240 240)
