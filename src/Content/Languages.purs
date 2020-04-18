module Languages where

import Category

import CSS as CSS
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem_)
import Prelude (map, discard)

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