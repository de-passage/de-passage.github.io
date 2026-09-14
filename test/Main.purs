module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Assets (resume)
import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (Either(..))
import Internationalization (Language(..), LocalizedString, translate)
import Main (languageParser, supportedLanguageParser)
import Parsing (runParser)

assert :: String -> Boolean -> Effect Unit
assert label condition = unless condition (throw label)

main :: Effect Unit
main = do
  assert "French query" $ runParser "lang=fr" languageParser == Right Fr
  assert "Japanese browser locale" $ runParser "ja-JP" supportedLanguageParser == Right Jp
  assert "English browser locale" $ runParser "en-US" supportedLanguageParser == Right En
  case runParser "lang=de" languageParser of
    Left _ -> pure unit
    Right _ -> throw "Unsupported languages must fall back"
  case jsonParser "{\"en\":\"Hello\",\"fr\":\"Bonjour\",\"ja\":null}" of
    Left error -> throw error
    Right json -> case decodeJson json of
      Left error -> throw (show error)
      Right (localized :: LocalizedString) -> do
        assert "English translation" $ translate En localized == "Hello"
        assert "French translation" $ translate Fr localized == "Bonjour"
        assert "Null translation falls back" $ translate Jp localized == "Hello"
  case jsonParser "{\"en\":\"Hello\"}" of
    Left error -> throw error
    Right json -> case decodeJson json of
      Left error -> throw (show error)
      Right (localized :: LocalizedString) ->
        assert "Missing translation falls back" $ translate Fr localized == "Hello"
  assert "French resume" $ resume Fr == "assets/resume_sylvain_leclercq_fr.pdf"
  assert "Japanese resume falls back to English" $ resume Jp == resume En
  log "Language parsing, localization, and resume tests passed."
