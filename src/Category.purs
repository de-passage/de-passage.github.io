module Category (category, categoryHidden, subcategory, subcategoryHidden) where

import Attributes

import CSS (CSS)
import CSS as CSS
import DOM.HTML.Indexed as I
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Prelude ((<>), discard)

category :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
category = categoryB true

categoryHidden :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
categoryHidden = categoryB false

categoryB :: forall w i. Boolean -> String -> String -> Array (HH.HTML w i) -> HH.HTML w i
categoryB = categoryRaw HH.h2

categoryRaw ::
  forall w i.
  (HH.Node I.HTMLh1 w i) ->
  Boolean ->
  String ->
  String ->
  Array (HH.HTML w i) ->
  HH.HTML w i
categoryRaw h b id title content =
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
          , HC.style categoryTitleStyle
          ]
          [ h
              [ HP.classes [ BS.cardHeader, (HH.ClassName "caret"), BS.alignBottom ] ]
              [ HH.text title ]
          ]
      , HH.div
          [ HP.classes (if b then [ BS.collapse, BS.show ] else [ BS.collapse ]), HP.id_ collapseId ]
          [ HH.div
              [ HP.classes [ BS.cardBody, BS.textCenter ] ]
              content
          ]
      ]

subcategoryB :: forall w i. Boolean -> String -> String -> Array (HH.HTML w i) -> HH.HTML w i
subcategoryB = categoryRaw HH.h4

subcategory :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
subcategory = subcategoryB true

subcategoryHidden :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i 
subcategoryHidden = subcategoryB false

categoryStyle :: CSS
categoryStyle = do CSS.color (CSS.black)

categoryTitleStyle :: CSS
categoryTitleStyle = do
  CSS.color (CSS.rgb 33 37 41)
  CSS.textDecoration CSS.noneTextDecoration
