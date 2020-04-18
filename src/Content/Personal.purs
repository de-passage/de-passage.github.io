module Personal where

import CSS as CSS
import Category (category)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Lists (listGroup, listItem)
import Prelude (map, (<>), discard)

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

    socialMedia r = HH.a [ HP.title r.title, HP.href r.url, mkClass r.id, HC.style linkStyle ] []
  in
    category "personal" "Sylvain Leclercq"
      [ listGroup
          [ listItem [ HC.style css ] (map socialMedia medias)
          ]
      ]