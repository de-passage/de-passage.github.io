module Languages where

import Attributes (dataBackdrop, dataDismiss, dataTarget, dataToggle)
import CSS as CSS
import CSS.Common (auto)
import CSS.Overflow as CSS.Overflow
import Category (categoryHidden, subcategory, subcategoryHidden)
import Data.Tuple.Nested (Tuple3, tuple3, (/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem_, ListItem)
import Prelude (discard, map, negate, ($), (*>), (<>))

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
    mkListItem (lang /\ prof /\ desc /\ _) =
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
            [ HH.div [ HP.classes [ BS.modalDialog, (HH.ClassName "modal-window") ] ]
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

    mkEducationItem :: Education w i -> ListItem w i
    mkEducationItem ed =
      listItem_
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.col12, BS.colLg6, BS.colXl ] ] [ HH.text ed.period ]
            , HH.div [ HP.classes [ BS.col12, BS.colLg6, BS.colXl ] ] ed.name
            , HH.div [ HP.classes [ BS.col12, BS.colXl ] ] ed.comment
            ]
        ]
  in
    categoryHidden "eduCategory" "Education"
      [ subcategory "languages" "Languages" [ listGroup (map mkListItem langs) ]
      , subcategoryHidden "education" "Formal Education"
          [ HH.div [ HC.style (CSS.Overflow.overflowX CSS.Overflow.overflowAuto *> CSS.maxHeight (CSS.vh 60.0)) ]
              [ listGroup $ map mkEducationItem education ]
          ]
      ]

type Education w i
  = { period :: String
    , name :: Array (HH.HTML w i)
    , comment :: Array (HH.HTML w i)
    }

education :: forall w i. Array (Education w i)
education =
  [ { period: "2010 - 2014"
    , name: [ HH.text "Ecole Supérieure de Commerce et de Management" ]
    , comment:
        [ HH.a [ HP.href "https://en.wikipedia.org/wiki/Grandes_%C3%A9coles" ] [ HH.text "Grande école" ]
        , HH.text " , business school, 4th and 5th semester spent as an exchange student in Meiji University, Japan"
        ]
    }
  , { period: "2008 - 2010"
    , name:
        [ HH.a [ HP.href "https://en.wikipedia.org/wiki/Classe_pr%C3%A9paratoire_aux_grandes_%C3%A9coles" ]
            [ HH.text "CPGE" ]
        ]
    , comment:
        [ HH.text "2 years of preparatory classes for the entrance exams to the french  "
        , HH.a [ HP.href "https://en.wikipedia.org/wiki/Grandes_%C3%A9coles" ] [ HH.text "grande écoles" ]
        ]
    }
  , { period: "2008"
    , name:
        [ HH.a [ HP.href "https://en.wikipedia.org/wiki/Baccalaur%C3%A9at" ]
            [ HH.text "Baccalauréat" ]
        ]
    , comment: [ HH.text "French high school degree" ]
    }
  ]
