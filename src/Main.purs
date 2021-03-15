module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (Json, caseJsonObject, decodeJson)
import Data.Either (Either(..), either, hush)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (drop, dropWhile)
import Effect (Effect)
import Effect.Class as H
import Effect.Class.Console (log)
import Foreign.Object (Object, empty, insert)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Internationalization (Language(..), supportedLanguages)
import Internationalization as I
import Resume as R
import State as S
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.String (string)
import Unsafe.Coerce (unsafeCoerce)
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
main = do
  q <- query
  let
    e = (runParser q languageParser)
  lang <- either selectFromBrowser pure e
  HA.runHalogenAff do
    content <- AX.get AXRF.json "/assets/content.json"
    body <- HA.awaitBody
    dic <- case content of
      Left error -> do
        H.liftEffect $ log $ "Content loading failed: " <> AX.printError error
        pure empty
      Right response -> do
        H.liftEffect
          $ do
              log $ "Content loading succeeded "
              log $ unsafeCoerce response.body
        pure $ parseContent response.body
    H.liftEffect $ log $ unsafeCoerce dic
    runUI R.component { language: lang, content: dic } body
  where
  selectFromBrowser :: forall a. a -> Effect Language
  selectFromBrowser _ = (browserLanguage <#> \l -> runParser l supportedLanguageParser) <#> either (pure En) (identity)

  parseContent :: Json -> S.Dictionary
  parseContent = caseJsonObject empty parseObjectContent

  parseObjectContent :: Object Json -> S.Dictionary
  parseObjectContent = foldrWithIndex insertInto empty

  insertInto :: String -> Json -> S.Dictionary -> S.Dictionary
  insertInto name json acc = case parseElements json of
    Nothing -> acc
    Just elem -> insert name elem acc

  parseElements :: Json -> Maybe I.LocalizedString
  parseElements json = hush $ decodeJson json
