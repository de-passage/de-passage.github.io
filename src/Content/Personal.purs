module Personal where

import Assets as A
import Attributes (scopeRow)
import CSS as CSS
import CSS.Common (auto, none)
import Category (category)
import DOM.HTML.Indexed (HTMLa)
import Data.MediaType as MT
import Format (h2, h5, para)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem)
import Modal (modal)
import Prelude (map, (<>), discard, (*>))
import State (Action, State, Localizer, languageSelection, localize)

type Media
  = { title :: String, id :: String, url :: String }

resumeL :: Localizer
resumeL = localize "resume"

downloadL :: Localizer
downloadL = localize "download"

downloadLongL :: Localizer
downloadLongL = localize "download-long"

settingsL :: Localizer
settingsL = localize "settings"

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

personalInformation :: forall w. State -> HH.HTML w Action
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

    bio = modalWindow "Biography" "info-circle" (linkStyle *> additionalStyle) "About me" (aboutMe model)

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

aboutMe :: forall w i. State -> Array (HH.HTML w i)
aboutMe model =
  [ HH.div [ HC.style (CSS.width (CSS.pct 100.0)) ]
      [ HH.img [ HP.class_ (HH.ClassName "bio-picture"), HP.src "/assets/me.jpg" ]
      , HH.p [ HC.style (CSS.float none) ]
          [ HH.text
              """After almost 15 years of programming as a passion, I decided a couple years 
        ago to follow my calling and become a full-time software engineer."""
          , HH.br_
          , HH.text
              """ I focus on using my long experience and modern tools to build high-reliability, 
      high-maintanability software. I have a particular fondness for C++, Haskell and other strongly typed languages
      but I have had the occasion to use a wide array of technologies and I am always happy to learn a new 
      skill for a job."""
          ]
      ]
  , HH.div_ [ resume model [] [ HH.text "Download resume as pdf" ] ]
  , HH.table [ HP.classes [ BS.tableStriped, BS.table ] ]
      [ HH.tbody_
          [ tableRow "Birthday" [ HH.text "26/03/1990" ]
          , tableRow "Email" [ HH.a [ HP.href mailMe ] [ HH.text "contact@sylvainleclercq.com" ] ]
          , tableRow "Address" [ HH.text "Montigny-le-Bretonneux, France" ]
          ]
      ]
  , h5 """Biography"""
  , para
      """ I wrote my first program during a boring math lecture in junior high school in France, around 2005. 
      I discovered that my TI-82 scientific calculator could be programmed using some limited variant of BASIC
      and I became obsessed with it. I learned as much as I could from the manual but quickly hit performance
      issues as I graduated from writing trivial math programs to attempting to build games."""
  , para
      """I used the limited Internet access that my family had at the time to research ways to circumvent the
      issue, only to realize that all the solutions that I could find did not apply to the machine I had. In the 
      process, I stumbled on the "Site du Zéro" (now OpenClassroom), the de-facto reference website for French speaking
      programming self-learners, where I learned first C, then C++, as those were the only two languages with 
      comprehensive explanations."""
  , para
      """From then on I kept studying various programming languages and computing science. However as I 
        reached the end of high school, I didn't see it as anything more than a hobby and I went on to study 
        management in university."""
  , para
      """I moved to Japan in early 2012 as an exchange student, and lived there until mid-2014. I then dropped out of 
      university as I realized that management wasn't my calling and went on to travel the world. From there, 
      I spent a year in South Korea, 5 months in Taiwan and a year in Australia, before going back to France in late 2017."""
  , para
      """This was also the moment I decided I would pursue software engineering as a career, after over 13 years of 
      self study. I worked for almost two years as a C++ and C# engineer on desktop applications at Nexter Systems, 
      before growing sufficiently dissatisfied with the management of the IT projects and quitting in December 2019."""
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
