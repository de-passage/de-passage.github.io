module Resume (component) where

import Content.Skills as S
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Bootstrap as BS
import Languages as L
import Personal as P
import Prelude (Unit, absurd, identity, unit, ($))
import Projects as Projects
import State (State, Input, Action(..))
import Work as W

type ChildSlots
  = W.ChildSlots
      ( S.ChildSlots
          ( P.ChildSlots
              ( L.ChildSlots
                  ( projects :: Projects.Slot Unit
                  )
              )
          )
      )

_projects :: Proxy "projects"
_projects = Proxy

component :: forall q o m. MonadAff m => H.Component q Input o m
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
        [ W.workExperience state
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
