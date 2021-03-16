module Personal where

import Assets as A
import Attributes (scopeRow)
import CSS as CSS
import CSS.Common (auto, none)
import Category (category)
import DOM.HTML.Indexed (HTMLa)
import Data.MediaType as MT
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Format (h2, h5, para)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem)
import Marked as M
import Modal (modal)
import Prelude (Unit, absurd, discard, map, unit, (*>), (<>))
import State (Action, State, Localizer, languageSelection, localize)

type Media
  = { title :: String, id :: String, url :: String }

type ChildSlots r
  = ( bioLong :: M.Slot Unit, bioShort :: M.Slot Unit | r )

_bioLong = SProxy :: SProxy "bioLong"

_bioShort = SProxy :: SProxy "bioShort"

resumeL :: Localizer
resumeL = localize "resume"

downloadL :: Localizer
downloadL = localize "download"

downloadLongL :: Localizer
downloadLongL = localize "download-long"

settingsL :: Localizer
settingsL = localize "settings"

bioLongL :: Localizer
bioLongL = localize "bio-long"

bioShortL :: Localizer
bioShortL = localize "bio-short"

bioTitleL :: Localizer
bioTitleL = localize "bio-title"

aboutMeL :: Localizer
aboutMeL = localize "about-me"

mailMe :: String
mailMe = "mailto:contact@sylvainleclercq.com"

medias :: Array Media
medias =
  [ { title: "Email"
    , id: "envelope"
    , url: mailMe
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

blog :: Media
blog =
  { title: "Blog"
  , id: "pencil"
  , url: "/blog"
  }

resume :: forall w i. State -> Array (HH.IProp HTMLa i) -> Array (HH.HTML w i) -> HH.HTML w i
resume model props =
  HH.a
    ( props
        <> [ HP.title (downloadL model)
          , HP.href (A.resume model.language)
          , HP.download ("Sylvain Leclercq " <> resumeL model)
          , HP.target "_blank"
          ]
    )

personalInformation :: forall r m. MonadEffect m => State -> HH.ComponentHTML Action (ChildSlots r) m
personalInformation model =
  let
    mkClass id = HP.class_ (HH.ClassName ("fa fa-" <> id <> " fa-2x"))

    css = do
      CSS.display CSS.flex
      CSS.justifyContent CSS.spaceAround
      CSS.paddingRight (CSS.px 0.0)
      CSS.paddingLeft (CSS.px 0.0)

    linkStyle = do
      CSS.textDecoration CSS.noneTextDecoration
      CSS.color CSS.black
      CSS.paddingRight (CSS.px 2.0)
      CSS.key (CSS.fromString "cursor") "pointer"

    socialMedia r = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, HC.style linkStyle, HP.target "_blank" ] []

    socialMediaS r s = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, HC.style (linkStyle *> s), HP.target "_blank" ] []

    additionalStyle = do
      CSS.margin auto auto auto auto
      CSS.fontSize (CSS.em 3.0)

    modalWindow btn icon style title content =
      modal btn
        ( \a ->
            HH.a
              ( [ ARIA.role "button"
                , HP.title btn
                , mkClass icon
                , HC.style style
                ]
                  <> a
              )
              []
        )
        ( \a ->
            [ HH.h3 ([ HC.style (CSS.margin auto auto auto auto) ] <> a)
                [ HH.text title ]
            ]
        )
        content
        model

    resumeTxt = resumeL model

    settingsText = settingsL model

    resumeModal = modalWindow resumeTxt "download" linkStyle resumeTxt (downloadWindow model)

    bio = modalWindow (bioTitleL model) "info-circle" (linkStyle *> additionalStyle) (aboutMeL model) (aboutMe model)

    settings = modalWindow settingsText "cog" linkStyle settingsText (settingsWindow model)
  in
    category "personal" "Sylvain Leclercq"
      [ listGroup
          [ listItem []
              [ HH.div [ HP.class_ BS.row ]
                  [ HH.div [ HP.classes [ BS.col ] ]
                      [ HH.img [ HP.class_ (HH.ClassName "profile-picture"), HP.src "/assets/me.jpg" ] ]
                  , HH.div [ HP.classes [ BS.col ], HC.style css ]
                      [ HH.div [ HC.style (CSS.display CSS.flex) ] [ socialMediaS blog additionalStyle ]
                      , bio
                      ]
                  ]
              ]
          , listItem [ HC.style css ] (map socialMedia medias <> [ resumeModal, settings ])
          ]
      ]

aboutMe :: forall m r. MonadEffect m => State -> Array (HH.ComponentHTML Action (ChildSlots r) m)
aboutMe model =
  [ HH.div [ HC.style (CSS.width (CSS.pct 100.0)) ]
      [ HH.img [ HP.class_ (HH.ClassName "bio-picture"), HP.src "/assets/me.jpg" ]
      , HH.slot _bioShort unit M.component { text: bioShortL model, id: "bio-short" } absurd
      ]
  , HH.div_ [ resume model [] [ HH.text "Download resume as pdf" ] ]
  , HH.table [ HP.classes [ BS.tableStriped, BS.table ] ]
      [ HH.tbody_
          [ tableRow "Birthday" [ HH.text "26/03/1990" ]
          , tableRow "Email" [ HH.a [ HP.href mailMe ] [ HH.text "contact@sylvainleclercq.com" ] ]
          , tableRow "Address" [ HH.text "Montigny-le-Bretonneux, France" ]
          ]
      ]
  , HH.slot _bioLong unit M.component { text: bioLongL model, id: "bio-long" } absurd
  ]

downloadWindow :: forall w. State -> Array (HH.HTML w Action)
downloadWindow model =
  let
    frame lang =
      HH.div [ HP.class_ BS.row ]
        [ HH.div [ HP.classes [ BS.col12 ] ]
            [ HH.embed
                [ HP.class_ (HH.ClassName "embedded-resume")
                , HP.src (A.resume lang)
                , HP.type_ (MT.MediaType "application/pdf")
                , (HP.attr (HH.AttrName "scrolling") "auto")
                , (HP.attr (HH.AttrName "frameborder") "0")
                ]
                []
            ]
        ]

    em1 = (CSS.em 1.0)

    padding = HC.style (CSS.padding em1 em1 em1 em1)
  in
    [ h2 "Download resume as PDF"
    , HH.div [ HP.class_ BS.row, padding ]
        [ HH.div
            [ HP.classes [ BS.col6, BS.colSm2, BS.textCenter ] ]
            [ languageSelection model.language
            ]
        , HH.div [ HP.classes [ BS.col6, BS.colSm10, BS.textLeft ] ]
            [ resume model [ HP.class_ BS.alignMiddle ] [ HH.text (downloadL model) ]
            ]
        ]
    , frame model.language
    ]

settingsWindow :: forall w. State -> Array (HH.HTML w Action)
settingsWindow model = [ h2 (settingsL model), languageSelection model.language ]

tableRow :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
tableRow title content =
  HH.tr [ HP.class_ (HH.ClassName "bio-table") ]
    [ HH.th [ scopeRow ] [ HH.text title ]
    , HH.td_ content
    ]
