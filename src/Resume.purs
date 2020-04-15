module Resume (component) where

import Prelude

import Assets as Assets
import CSS (CSS, i)
import CSS as CSS
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML (IProp)
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

dataToggleAN :: H.AttrName
dataToggleAN = H.AttrName "data-toggle"

dataTargetAN :: H.AttrName
dataTargetAN = H.AttrName "data-target"

dataBackdropAN :: H.AttrName
dataBackdropAN = H.AttrName "data-backdrop"

dataDismissAN :: H.AttrName
dataDismissAN = H.AttrName "data-dismiss"

dataToggle :: forall w i. String -> IProp w i
dataToggle = HP.attr dataToggleAN

dataTarget :: forall w i. String -> IProp w i
dataTarget = HP.attr dataTargetAN

dataBackdrop :: forall w i. String -> IProp w i
dataBackdrop = HP.attr dataBackdropAN

dataDismiss :: forall w i. String -> IProp w i
dataDismiss = HP.attr dataDismissAN

collapseAttr :: forall p i. HP.IProp p i
collapseAttr = dataToggle "collapse"

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

newtype ListItem w i
  = ListItem (HH.HTML w i)

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
        , listItem
            [ HH.a
                [ HP.href "mailto:contact@sylvainleclercq.com" ]
                [ HH.text "contact@sylvainleclercq.com" ]
            ]
        ]
    ]

mkSkillLink :: forall w i. String -> Assets.Icon w i -> Assets.Icon w i
mkSkillLink id ic = Assets.decorate withLink ic
  where
  st :: CSS.CSS
  st = CSS.display CSS.flex

  withLink :: forall w' i'. HH.HTML w' i' -> HH.HTML w' i'
  withLink icon =
    HH.div
      [ HP.class_ BS.card, style (CSS.display CSS.inlineBlock) ]
      [ HH.button
          [ dataToggle "modal"
          , dataTarget ("#modal" <> id)
          , style st
          ]
          [ icon
          ]
      , HH.div
          [ HP.classes [ BS.modal, BS.fade ]
          , HP.id_ ("modal" <> id)
          , HP.tabIndex (-1)
          , dataBackdrop "static"
          , ARIA.role "dialog"
          , ARIA.labelledBy ("backdropLabel" <> id)
          , ARIA.hidden "true"
          ]
          [ HH.div [ HP.class_ BS.modalDialog ]
              [ HH.div [ HP.class_ BS.modalContent ]
                  [  HH.div [ HP.class_ BS.modalHeader ]
                        [ HH.h5 [ HP.class_ BS.modalTitle, HP.id_ ("backdropLabel" <> id) ] [ HH.text id ]
                        ]
                    , HH.div [HP.class_ BS.modalBody ] []
                    , HH.div [HP.class_ BS.modalFooter ] [
                      HH.button [HP.class_ BS.btnOutlinePrimary, dataDismiss "modal", HP.type_ HP.ButtonButton] [HH.text "Close"]
                    ] 
                  ]
              ]
          ]
      ]

technicalSkills :: forall w i. HH.HTML w i
technicalSkills =
  category "skills" "Technical skills"
    [ Assets.iconList_
        $ map (\(Tuple s i) -> mkSkillLink s i)
            [ (Tuple "purescript" Assets.purescriptIcon)
            , (Tuple "elm" Assets.elmIcon)
            , (Tuple "cpp" Assets.cppIcon)
            , (Tuple "haskell" Assets.haskellIcon)
            , (Tuple "c" Assets.cIcon)
            , (Tuple "lua" Assets.luaIcon)
            , (Tuple "js" Assets.javascriptIcon)
            , (Tuple "csharp" Assets.csharpIcon)
            , (Tuple "coffeescript" Assets.coffeescriptIcon)
            , (Tuple "ruby" Assets.rubyIcon)
            , (Tuple "python" Assets.pythonIcon)
            , (Tuple "html" Assets.htmlIcon)
            , (Tuple "css" Assets.cssIcon)
            ]
    ]
