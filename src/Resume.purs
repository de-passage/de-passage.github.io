module Resume (component) where

import Prelude

import Assets as Assets
import CSS (CSS)
import CSS as CSS
import CSS.Common (auto) as CSS
import Content.Skills as S
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
render _ =
  HH.div [ HP.class_ BS.accordion ]
    [ HH.div [ HP.class_ BS.cardDeck ] categories ]

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

medias :: Array { title :: String, id :: String, url :: String }
medias =
  [ { title: "LinkedIn"
    , id: "linkedin"
    , url: "https://www.linkedin.com/in/sylvain-leclercq-12b933154/"
    }
  , { title: "GitHub"
    , id: "github"
    , url: "https://github.com/de-passage"
    }
  ]

personalInformation :: forall w i. HH.HTML w i
personalInformation =
  let
    socialMedia r = HH.a [ HP.title r.title, HP.href r.url ] [ Assets.icon r.id 4.0 ]
  in
    category "personal" "Personal Information"
      [ listGroup
          [ listItem [ HH.h3_ [ HH.text "Sylvain Leclercq" ] ]
          , listItem
              [ HH.a
                  [ HP.href "mailto:contact@sylvainleclercq.com", HP.class_ BS.fontWeightBold ]
                  [ HH.text "contact@sylvainleclercq.com" ]
              ]
          , listItem (map socialMedia medias)
          ]
      ]

mkSkillLink :: forall w i. String -> S.SkillDescription w i -> HH.HTML w i
mkSkillLink id = withLink
  where
  st :: CSS
  st = CSS.display CSS.flex

  withLink :: forall w' i'. S.SkillDescription w' i' -> HH.HTML w' i'
  withLink desc =
    HH.div
      [ HP.class_ BS.card, style (CSS.display CSS.inlineBlock) ]
      [ HH.button
          [ dataToggle "modal"
          , dataTarget ("#modal" <> id)
          , style st
          ]
          [ desc.icon 7.0
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
                  [ HH.div [ HP.class_ BS.modalHeader ]
                      [ desc.icon 5.0
                      , HH.h3
                          [ HP.class_ BS.modalTitle
                          , style (CSS.margin CSS.auto CSS.auto CSS.auto CSS.auto)
                          , HP.id_ ("backdropLabel" <> id)
                          ]
                          [ HH.text desc.title ]
                      ]
                  , HH.div [ HP.class_ BS.modalBody ] [ desc.content ]
                  , HH.div [ HP.class_ BS.modalFooter ]
                      [ HH.button
                          [ HP.classes [ BS.btn, BS.btnOutlinePrimary ]
                          , dataDismiss "modal"
                          , HP.type_ HP.ButtonButton
                          ]
                          [ HH.text "Close" ]
                      ]
                  ]
              ]
          ]
      ]

technicalSkills :: forall w i. HH.HTML w i
technicalSkills =
  category "skills" "Technical skills"
    [ HH.div [ style (CSS.justifyContent CSS.spaceAround) ]
        $ map (\(Tuple s i) -> mkSkillLink s i)
            [ (Tuple "purescript" S.purescript)
            , (Tuple "elm" S.elm)
            , (Tuple "cpp" S.cpp)
            , (Tuple "haskell" S.haskell)
            , (Tuple "c" S.c)
            , (Tuple "lua" S.lua)
            , (Tuple "js" S.javascript)
            , (Tuple "csharp" S.csharp)
            , (Tuple "coffeescript" S.coffeescript)
            , (Tuple "ruby" S.ruby)
            , (Tuple "python" S.python)
            , (Tuple "html" S.html)
            , (Tuple "css" S.css)
            ]
    ]
