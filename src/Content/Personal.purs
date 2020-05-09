module Personal where

import Assets as A
import Attributes (scopeRow)
import CSS as CSS
import CSS.Common (auto, none)
import Category (category)
import DOM.HTML.Indexed (HTMLa)
import Data.Array (snoc)
import Format (h5, para)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem)
import Modal (modal)
import Prelude (map, (<>), discard, (*>))

type Media
  = { title :: String, id :: String, url :: String }

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
  , url: "#"
  }

resume :: forall w i. Array (HH.IProp HTMLa i) -> Array (HH.HTML w i) -> HH.HTML w i
resume props =
  HH.a
    ( props
        <> [ HP.title "Download"
          , HP.href A.resume
          , HP.download "Sylvain Leclercq"
          , HP.target "_blank"
          ]
    )

personalInformation :: forall w i. HH.HTML w i
personalInformation =
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

    socialMediaS :: Media -> CSS.CSS -> HH.HTML w i
    socialMediaS r s = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, HC.style (linkStyle *> s), HP.target "_blank" ] []

    additionalStyle = do
      CSS.margin auto auto auto auto
      CSS.fontSize (CSS.em 3.0)

    dl =
      resume
        [ mkClass "download"
        , HC.style linkStyle
        ]
        []
  in
    category "personal" "Sylvain Leclercq"
      [ listGroup
          [ listItem []
              [ HH.div [ HP.class_ BS.row ]
                  [ HH.div [ HP.classes [ BS.col ] ]
                      [ HH.img [ HP.class_ (HH.ClassName "profile-picture"), HP.src "/assets/me.jpg" ] ]
                  , HH.div [ HP.classes [ BS.col ], HC.style css ]
                      [ {- socialMediaS blog additionalStyle, -} modal "Bio"
                          ( \a ->
                              HH.a
                                ( [ ARIA.role "button"
                                  , HP.title "Biography"
                                  , mkClass "info-circle"
                                  , HC.style (linkStyle *> additionalStyle)
                                  ]
                                    <> a
                                )
                                []
                          )
                          ( \a ->
                              [ HH.h3 ([ HC.style (CSS.margin auto auto auto auto) ] <> a)
                                  [ HH.text "About me" ]
                              ]
                          )
                          aboutMe
                      ]
                  ]
              ]
          , listItem [ HC.style css ] (map socialMedia medias `snoc` dl)
          ]
      ]

aboutMe :: forall w i. Array (HH.HTML w i)
aboutMe =
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
  , HH.div_ [ resume [] [ HH.text "Download resume as pdf" ] ]
  , HH.table [ HP.classes [ BS.tableStriped, BS.table ] ]
      [ HH.tbody_
          [ tableRow "Birthday" [ HH.text "26/03/1990" ]
          , tableRow "Email" [ HH.a [ HP.href mailMe ] [ HH.text "contact@sylvainleclercq.com" ] ]
          , tableRow "Address" [ HH.text "Kowloon City, Hong Kong SAR" ]
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

tableRow :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
tableRow title content =
  HH.tr [ HP.class_ (HH.ClassName "bio-table") ]
    [ HH.th [ scopeRow ] [ HH.text title ]
    , HH.td_ content
    ]
