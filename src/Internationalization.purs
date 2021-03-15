module Internationalization (Language(..), class Translate, translate, supportedLanguages, DownloadText(..)) where

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Maybe (Maybe, fromMaybe)
import Prelude (class Eq, class Show)

data Language
  = En
  | Fr
  | Jp

newtype LocalizedString
  = LocalizedString
  { en :: String
  , fr :: Maybe String
  , ja :: Maybe String
  }

class Translate a where
  translate :: Language -> a -> String

instance showLanguage :: Show Language where
  show En = "en"
  show Fr = "fr"
  show Jp = "ja"

instance translateLanguage :: Translate Language where
  translate _ En = "English"
  translate _ Jp = "日本語"
  translate _ Fr = "Français"

derive instance eqLanguage :: Eq Language

supportedLanguages :: Array Language
supportedLanguages = [ En, Fr ]

data DownloadText
  = DownloadText

instance translateDownloadText :: Translate DownloadText where
  translate En _ = "Download"
  translate Fr _ = "Télécharger"
  translate Jp _ = "ダウンロード"

instance translateLocalizedString :: Translate LocalizedString where
  translate En (LocalizedString str) = str.en
  translate Fr (LocalizedString str) = fromMaybe str.en str.fr
  translate Jp (LocalizedString str) = fromMaybe str.en str.ja

instance decodeJsonLocalizedString :: DecodeJson LocalizedString where
  decodeJson str = rmap LocalizedString (decodeJson str)
