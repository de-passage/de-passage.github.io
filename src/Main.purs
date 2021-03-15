module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (stringify)
import Data.Either (Either(..), either)
import Data.String.CodeUnits (drop, dropWhile)
import Effect (Effect)
import Effect.Class as H
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Internationalization (Language(..), supportedLanguages)
import Resume as R
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.String (string)
import Web.HTML (window) as DOM
import Web.HTML.Location (search) as DOM
import Web.HTML.Navigator (language) as DOM
import Web.HTML.Window (location, navigator) as DOM

specLang :: String -> Language -> Parser String Language
specLang txt lang = string txt $> lang

supportedLanguageParser :: Parser String Language
supportedLanguageParser = choice (map parser supportedLanguages)
  where
  parser lang = specLang (show lang) lang

languageParser :: Parser String Language
languageParser = string "lang=" *> supportedLanguageParser

hash :: Effect String
hash = DOM.window >>= DOM.location >>= DOM.search

query :: Effect String
query = hash <#> dropWhile (\c -> c /= '?') <#> drop 1

browserLanguage :: Effect String
browserLanguage = DOM.window >>= DOM.navigator >>= DOM.language

main :: Effect Unit
main =
  HA.runHalogenAff do
    q <- H.liftEffect query
    let
      e = (runParser q languageParser)
    lang <- H.liftEffect $ either selectFromBrowser pure e
    body <- HA.awaitBody
    runUI R.component lang body
  where
  selectFromBrowser :: forall a. a -> Effect Language
  selectFromBrowser _ = (browserLanguage <#> \l -> runParser l supportedLanguageParser) <#> either (pure En) (identity)
