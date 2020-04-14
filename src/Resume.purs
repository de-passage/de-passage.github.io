module Resume (component) where

import Prelude

import CSS (CSS)
import CSS as CSS
import Data.Newtype (class Newtype, unwrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS

type State
  = Unit

type Action
  = Void

type ChildSlots
  = ()

dataToggle :: H.AttrName
dataToggle = H.AttrName "data-toggle"

collapseAttr :: forall p i. HP.IProp p i
collapseAttr = HP.attr dataToggle "collapse"

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render _ = HH.div [ style globalStyle, HP.class_ BS.accordion ] categories

handleAction :: forall o m. Unit -> H.HalogenM State Action ChildSlots o m Unit
handleAction _ = pure unit

globalStyle :: CSS
globalStyle = do CSS.backgroundColor (CSS.rgb 240 240 240)

categories :: forall w i. Array (HH.HTML w i)
categories = [ personalInformation, technicalSkills ]

category :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
category id title content =
  let
    collapseId = "collapse" <> id
  in
    HH.div [ HP.class_ BS.card ]
      [ HH.a
          [ collapseAttr
          , HP.href ("#" <> collapseId)
          , ARIA.role "button"
          , ARIA.expanded "false"
          , ARIA.controls collapseId
          , style categoryTitleStyle
          ]
          [ HH.h2
              [ HP.classes [ BS.cardHeader ] ]
              [ HH.text title ]
          ]
      , HH.div
          [ HP.classes [ BS.collapse, BS.show ], HP.id_ collapseId ]
          [ HH.div
              [ HP.class_ BS.cardBody ]
              content
          ]
      ]

categoryStyle :: CSS
categoryStyle = do CSS.color (CSS.black)

categoryTitleStyle :: CSS
categoryTitleStyle = do
  CSS.color (CSS.rgb 33 37 41)
  CSS.textDecoration CSS.noneTextDecoration

newtype ListItem w i = ListItem (HH.HTML w i)
derive instance newtypeListItem :: Newtype (ListItem w i) _

listGroup :: forall w i. Array (ListItem w i) -> HH.HTML w i
listGroup = map unwrap >>> HH.ul [ HP.classes [ BS.listGroup, BS.listGroupFlush ] ]

listItem :: forall w i. Array (HH.HTML w i) -> ListItem w i
listItem = HH.li [ HP.class_ BS.listGroupItem ] >>> ListItem


personalInformation :: forall w i. HH.HTML w i
personalInformation =
  category "personal" "Personal Information"
    [ listGroup
        [ listItem [ HH.text "Sylvain Leclercq" ]
        , listItem [ HH.text "contact@sylvainleclercq.com" ]
        ]
    ]

technicalSkills :: forall w i. HH.HTML w i
technicalSkills =
  category "skills" "Technical skills"
    [ listGroup
        [ listItem [ HH.text "Purescript" ]
        , listItem [ HH.text "HTML" ]
        ]
    ]
