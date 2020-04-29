module Modal where

import Prelude
import Attributes (dataBackdrop, dataDismiss, dataTarget, dataToggle)
import DOM.HTML.Indexed (HTMLh1)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS

modal ::
  forall r w i.
  String ->
  (Array (HH.IProp r i) -> HH.HTML w i) ->
  (Array (HH.IProp HTMLh1 i) -> Array (HH.HTML w i)) ->
  Array (HH.HTML w i) ->
  HH.HTML w i
modal id btn title content =
  HH.div_
    [ btn [ dataToggle "modal", dataTarget ("#modal" <> id) ]
    , HH.div
        [ HP.classes [ BS.modal, BS.fade ]
        , HP.id_ ("modal" <> id)
        , HP.tabIndex (-1)
        , dataBackdrop "static"
        , ARIA.role "dialog"
        , ARIA.labelledBy ("backdropLabel" <> id)
        , ARIA.hidden "true"
        ]
        [ HH.div [ HP.classes [ BS.modalDialog, (HH.ClassName "modal-window") ] ]
            [ HH.div [ HP.class_ BS.modalContent ]
                [ HH.div [ HP.class_ BS.modalHeader ]
                    $ title
                        [ HP.class_ BS.modalTitle
                        , HP.id_ ("backdropLabel" <> id)
                        ]
                , HH.div [ HP.classes [ BS.modalBody, BS.textJustify ] ] content
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
