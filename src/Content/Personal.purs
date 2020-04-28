module Personal where

import CSS as CSS
import CSS.Common (auto)
import Category (category)
import Data.Array (snoc)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Lists (listGroup, listItem)
import Prelude (map, (<>), discard, (*>))
import Halogen.Themes.Bootstrap4 as BS

type Media
  = { title :: String, id :: String, url :: String }

medias :: Array Media
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

blog :: Media
blog =
  { title: "Blog"
  , id: "pencil"
  , url: "#"
  }

info :: Media
info =
  { title: "Presentation"
  , id: "info-circle"
  , url: "#"
  }

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

    socialMedia r = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, HC.style linkStyle ] []

    socialMediaS :: Media -> CSS.CSS -> HH.HTML w i
    socialMediaS r s = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, HC.style (linkStyle *> s) ] []

    additionalStyle :: CSS.CSS
    additionalStyle = CSS.margin auto auto auto auto

    dl =
      HH.a
        [ HP.title "Download"
        , HP.href "resume.pdf"
        , HP.download "download"
        , mkClass "download"
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
                    [ {- socialMediaS blog additionalStyle, -} socialMediaS info additionalStyle ]
                  ]
              ]
          , listItem [ HC.style css ] (map socialMedia medias `snoc` dl)
          ]
      ]
