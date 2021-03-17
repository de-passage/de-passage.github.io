module Languages where

import CSS as CSS
import CSS.Common (auto)
import CSS.Overflow as CSS.Overflow
import Category (categoryHidden, subcategory, subcategoryHidden)
import Data.Array (mapWithIndex)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (Tuple3, tuple3, (/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Lists (listGroup, listItem_, ListItem)
import Marked as M
import Modal (modal)
import Prelude (absurd, discard, ($), (*>), (<>))
import State (State, localize)

type ChildSlots r
  = ( modalLang :: M.Slot Int, educationDescription :: M.Slot Int, educationTitle :: M.Slot Int | r )

_modalLang = SProxy :: SProxy "modalLang"

_educationDescription = SProxy :: SProxy "educationDescription"

_educationTitle = SProxy :: SProxy "educationTitle"

languages :: forall m a r. MonadEffect m => State -> HH.ComponentHTML a (ChildSlots r) m
languages model =
  let
    css = do
      CSS.display CSS.inlineBlock
      CSS.paddingRight (CSS.px 4.0)

    langs :: Array (Tuple3 String String String)
    langs =
      [ tuple3 "french" "native" "french-description"
      , tuple3 "english" "near-bilingual" "english-description"
      , tuple3 "japanese" "fluent" "japanese-description"
      ]

    mkListItem :: Int -> Tuple3 String String String -> ListItem (H.ComponentSlot HH.HTML (ChildSlots r) m a) a
    mkListItem idx (lang /\ prof /\ desc /\ _) =
      listItem_
        [ modal lang
            ( \a ->
                HH.button
                  ( [ HP.classes [ BS.btn, (H.ClassName "invisible-btn") ]
                    , HP.title (localize lang model)
                    ]
                      <> a
                  )
                  [ HH.div [ HP.class_ BS.row ]
                      [ HH.div [ HP.classes [ BS.textLeft, BS.col ] ]
                          [ HH.text (localize lang model) ]
                      , HH.div [ HP.classes [ BS.textRight, BS.col ] ]
                          [ HH.text (localize prof model) ]
                      , HH.div_ [ HH.i [ HP.classes [ BS.alignMiddle, (H.ClassName "fa fa-info-circle") ] ] [] ]
                      ]
                  ]
            )
            ( \a ->
                [ HH.h3 a [ HH.text (localize lang model) ]
                , HH.h4 [ HC.style (CSS.marginTop auto) ] [ HH.text (localize prof model) ]
                ]
            )
            [ HH.slot _modalLang idx M.component { text: localize desc model, id: desc } absurd ]
            model
        ]

    mkEducationItem :: Int -> Education -> ListItem (H.ComponentSlot HH.HTML (ChildSlots r) m a) a
    mkEducationItem idx ed =
      listItem_
        [ HH.div [ HP.classes [ BS.row ] ]
            [ HH.div [ HP.classes [ BS.col12, BS.colLg6, BS.fontWeightBold ] ] [ HH.text ed.period ]
            , HH.div [ HP.classes [ BS.col12, BS.colLg6, BS.fontWeightBold ] ] [ HH.slot _educationTitle idx M.component { text: localize ed.name model, id: ed.name } absurd ]
            , HH.div [ HP.classes [ BS.col12, BS.colXl, BS.textJustify ] ] [ HH.slot _educationDescription idx M.component { text: localize ed.comment model, id: ed.comment } absurd ]
            ]
        ]
  in
    categoryHidden "eduCategory" (localize "education" model)
      [ subcategory "languages" (localize "languages" model) [ listGroup (mapWithIndex mkListItem langs) ]
      , subcategoryHidden "education" (localize "formal-education" model)
          [ HH.div [ HC.style (CSS.Overflow.overflowX CSS.Overflow.overflowAuto *> CSS.maxHeight (CSS.vh 60.0)) ]
              [ listGroup $ mapWithIndex mkEducationItem education ]
          ]
      ]

type Education
  = { period :: String
    , name :: String
    , comment :: String
    }

education :: Array Education
education =
  [ { period: "2010 - 2014"
    , name: "escem-title"
    , comment: "escem-description"
    }
  , { period: "2008 - 2010"
    , name: "cpge-title"
    , comment: "cpge-description"
    }
  , { period: "2008"
    , name: "bac-title"
    , comment: "bac-description"
    }
  ]
