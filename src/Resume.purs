module Resume (component) where

import Prelude
import CSS (CSS)
import CSS as CSS
import CSS.Common (auto, none) as CSS
import Content.Skills as S
import Data.Array (snoc)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 (alignItemsStart)
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
  let
    personal = HH.div [ HP.classes [ BS.colMd4, BS.colSm5, BS.col12 ] ] [ personalInformation, languages ]

    skill = HH.div [ HP.classes [ BS.colMd8, BS.colSm7, BS.col12 ] ] [ technicalSkills ]
  in
    HH.div [ HP.class_ BS.container ]
      [ HH.div [ HP.class_ BS.row ]
          [ personal
          , skill
          ]
      ]

handleAction :: forall o m. Unit -> H.HalogenM State Action ChildSlots o m Unit
handleAction _ = pure unit

globalStyle :: CSS
globalStyle = do CSS.backgroundColor (CSS.rgb 240 240 240)

category :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
category = categoryB true

categoryHidden :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
categoryHidden = categoryB false

categoryB :: forall w i. Boolean -> String -> String -> Array (HH.HTML w i) -> HH.HTML w i
categoryB b id title content =
  let
    collapseId = "collapse" <> id
  in
    HH.div [ HP.class_ BS.card ]
      [ HH.a
          [ collapseAttr
          , HP.href ("#" <> collapseId)
          , ARIA.role "button"
          , ARIA.expanded (if b then "true" else "false")
          , ARIA.controls collapseId
          , style categoryTitleStyle
          ]
          [ HH.h2
              [ HP.classes [ BS.cardHeader ] ]
              [ HH.text title ]
          ]
      , HH.div
          [ HP.classes (if b then [ BS.collapse, BS.show ] else [ BS.collapse ]), HP.id_ collapseId ]
          [ HH.div
              [ HP.classes [ BS.cardBody, BS.textCenter ] ]
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

listGroupC :: forall w i. Array H.ClassName -> Array (ListItem w i) -> HH.HTML w i
listGroupC classes = map unwrap >>> HH.ul [ HP.classes $ classes <> [ BS.listGroup, BS.listGroupFlush ] ]

type AH w i
  = Array (HH.HTML w i)

listItem props = HH.li (props <> [ HP.class_ BS.listGroupItem ]) >>> ListItem

listItem_ :: forall w i. Array (HH.HTML w i) -> ListItem w i
listItem_ = listItem []

medias :: Array { title :: String, id :: String, url :: String }
medias =
  [ { title: "Email"
    , id: "envelope"
    , url: "mailto:contact@sylvainleclercq.com"
    }
  , { title: "GitHub"
    , id: "github"
    , url: "https://github.com/de-passage"
    }
  , { title: "LinkedIn"
    , id: "linkedin"
    , url: "https://www.linkedin.com/in/sylvain-leclercq-12b933154/"
    }
  ]

personalInformation :: forall w i. HH.HTML w i
personalInformation =
  let
    mkClass id = HP.class_ (HH.ClassName ("fa fa-" <> id <> " fa-2x"))

    css = do
      CSS.display CSS.flex
      CSS.justifyContent CSS.spaceAround

    linkStyle = do
      CSS.textDecoration CSS.noneTextDecoration
      CSS.color CSS.black

    socialMedia r = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, style linkStyle ] []
  in
    category "personal" "Sylvain Leclercq"
      [ listGroup
          [ listItem [ style css ] (map socialMedia medias)
          ]
      ]

mkSkillLink :: forall w i. String -> S.SkillDescription w i -> HH.HTML w i
mkSkillLink id desc =
  HH.div
    [ HP.class_ BS.card, style (CSS.display CSS.inlineBlock) ]
    [ HH.button
        [ dataToggle "modal"
        , dataTarget ("#modal" <> id)
        , HP.title desc.title
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

languages :: forall w i. HH.HTML w i
languages =
  let
    css = do
      CSS.display CSS.inlineBlock
      CSS.paddingRight (CSS.px 4.0)

    langs =
      [ Tuple "French" "Native"
      , Tuple "English" "Near Bilingual"
      , Tuple "Japanese" "Fluent"
      ]

    mkListItem (Tuple lang prof) =
      listItem_
        [ HH.button [ HP.class_ (H.ClassName "invisible-btn") ]
            [ HH.div [ HP.class_ BS.row ]
                [ HH.div [ HP.classes [ BS.textLeft, BS.col ] ]
                    [ HH.text lang ]
                , HH.div [ HP.classes [ BS.textRight, BS.col ] ]
                    [ HH.text prof ]
                , HH.div [ HP.classes [] ] [ HH.i [ HP.classes [ (H.ClassName "fa fa-info-circle") ] ] [] ]
                ]
            ]
        ]
  in
    categoryHidden "languages" "Languages"
      [ listGroup (map mkListItem langs)
      ]
