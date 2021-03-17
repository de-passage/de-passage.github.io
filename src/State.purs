module State (State(..), Action(..), Dictionary(..), Input(..), languageSelection, localize, Localizer(..)) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object (lookup)
import Foreign.Object as FO
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Internationalization as I

type Dictionary
  = FO.Object I.LocalizedString

type State
  = { language :: I.Language
    , content :: Dictionary
    }

type Input
  = State

data Action
  = LanguageChanged I.Language

type Localizer
  = State -> String

languageSelection :: forall w. I.Language -> HH.HTML w Action
languageSelection currentLanguage =
  let
    elId = "languageSelectDropdown"

    ddItem lang = HH.a [ HP.class_ BS.dropdownItem, HP.href "#", HE.onClick (\_ -> Just (LanguageChanged lang)) ] [ HH.text (I.translate currentLanguage lang) ]

    remainingLanguages = map ddItem (Array.delete currentLanguage I.supportedLanguages)
  in
    HH.div [ HP.class_ BS.dropdown ]
      [ HH.button
          [ HP.classes [ BS.btn, BS.btnOutlinePrimary, BS.dropdownToggle ]
          , HP.type_ HP.ButtonButton
          , HP.id_ elId
          , ARIA.hasPopup "true"
          , ARIA.expanded "false"
          , HP.attr (HH.AttrName "data-toggle") "dropdown"
          ]
          [ HH.text (I.translate currentLanguage currentLanguage)
          ]
      , HH.div [ HP.class_ BS.dropdownMenu, ARIA.labelledBy elId ] remainingLanguages
      ]

localize :: String -> State -> String
localize key model = fromMaybe ("{string missing: " <> key <> "}") $ I.translate model.language <$> lookup key model.content
