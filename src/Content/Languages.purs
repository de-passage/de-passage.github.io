module Languages where

import Attributes (dataBackdrop, dataDismiss, dataTarget, dataToggle)
import CSS as CSS
import CSS.Common (auto)
import Category (categoryHidden)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem_, ListItem)
import Prelude ((<>), map, discard, negate)

languages :: forall w i. HH.HTML w i
languages =
  let
    css = do
      CSS.display CSS.inlineBlock
      CSS.paddingRight (CSS.px 4.0)

    langs :: forall r j. Array (Tuple3 String String (Array (HH.HTML r j)))
    langs =
      [ tuple3 "French" "Native"
          [ HH.p [ HP.class_ BS.textJustify ]
              [ HH.text
                  """I was born and raised in France, near Paris.
               Although my website is in English and I don't use French much anymore,
                it is still my mother tongue."""
              ]
          ]
      , tuple3 "English" "Near Bilingual"
          [ HH.p_
              [ HH.text
                  """ I learned English as a teenager watching American TV shows. As my
                  interests expanded into programming and I started living abroad, I naturally 
                  had both the incentive and the opportunity to improve my language abilities by
                  reading technical litterature, watching various online content and interacting
                  in English with others.
                  """
              ]
          , HH.p_ [ HH.text "Nowadays, most of my life is conducted in English." ]
          , HH.h6_ [ HH.text "Certifications" ]
          , HH.p_ [ HH.text "TOIEC Passed in 2013 with 990 points." ]
          ]
      , tuple3 "Japanese" "Fluent"
          [ HH.p_
              [ HH.text
                  """ I started learning Japanese at around 16 years old, but my language
                  ability only really took off during my studies at Meiji University, Japan,
                  between 2012 and 2014. I studied on my own and achieved a fairly high level
                  by the time I left Japan.
                  """
              ]
          , HH.p_
              [ HH.text
                  """ I have since then done my best to maintain fluency both in speaking and writting.
                  """
              ]
          , HH.h6_ [ HH.text "Certifications" ]
          , HH.p_ [ HH.text "JLPT N1 Passed in 2014." ]
          ]
      ]

    mkListItem :: forall r j. Tuple3 String String (Array (HH.HTML r j)) -> ListItem r j
    mkListItem (Tuple lang (Tuple prof (Tuple desc _))) =
      listItem_
        [ HH.button
            [ HP.classes [ BS.btn, (H.ClassName "invisible-btn") ]
            , dataToggle "modal"
            , dataTarget ("#modal" <> lang)
            , HP.title lang
            ]
            [ HH.div [ HP.class_ BS.row ]
                [ HH.div [ HP.classes [ BS.textLeft, BS.col ] ]
                    [ HH.text lang ]
                , HH.div [ HP.classes [ BS.textRight, BS.col ] ]
                    [ HH.text prof ]
                , HH.div_ [ HH.i [ HP.classes [ BS.alignMiddle, (H.ClassName "fa fa-info-circle") ] ] [] ]
                ]
            ]
        , HH.div
            [ HP.classes [ BS.modal, BS.fade ]
            , HP.id_ ("modal" <> lang)
            , HP.tabIndex (-1)
            , dataBackdrop "static"
            , ARIA.role "dialog"
            , ARIA.labelledBy ("backdropLabel" <> lang)
            , ARIA.hidden "true"
            ]
            [ HH.div [ HP.class_ BS.modalDialog ]
                [ HH.div [ HP.class_ BS.modalContent ]
                    [ HH.div [ HP.class_ BS.modalHeader ]
                        [ HH.h3
                            [ HP.class_ BS.modalTitle
                            , HP.id_ ("backdropLabel" <> lang)
                            ]
                            [ HH.text lang ]
                        , HH.h4 [ HC.style (CSS.marginTop auto) ] [ HH.text prof ]
                        ]
                    , HH.div [ HP.classes [ BS.modalBody, BS.textJustify ] ] desc
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
  in
    categoryHidden "languages" "Languages"
      [ listGroup (map mkListItem langs)
      ]
