module Content.Skills
  ( purescript
  , elm
  , c
  , cpp
  , haskell
  , lua
  , javascript
  , csharp
  , coffeescript
  , ruby
  , python
  , html
  , css
  , SkillDescription
  , technicalSkills
  , Source(..)
  , Quote(..)
  , UrlSource(..)
  , ChildSlots(..)
  ) where

import Assets as A
import CSS as CSS
import CSS.Common (auto)
import Category (category, subcategory, subcategoryHidden)
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Format (para)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Marked as M
import Modal (modal)
import Prelude (absurd, discard, map, ($))
import State (Localizer, State, localize)

type ChildSlots r
  = ( modalContent :: M.Slot String | r )

_modalContent = SProxy :: SProxy "modalContent"

type UrlSource
  = { url :: String
    , name :: String
    }

data Source
  = Source String
  | USource UrlSource

type Quote
  = { text :: String
    , source :: Source
    }

type SkillDescription
  = { icon :: String
    , title :: String
    , content :: String
    , url :: String
    , quote :: Maybe Quote
    }

purescript :: SkillDescription
purescript =
  { icon: A.purescriptIcon
  , title: "Purescript"
  , url: "https://purescript.org"
  , content: "purescript-modal"
  , quote:
      Just
        { text: "Purescript is a strongly-typed functional programming language that compiles to JavaScript."
        , source:
            USource
              { url: "https://purescript.org"
              , name: "purescript.org"
              }
        }
  }

elm :: SkillDescription
elm =
  { icon: A.elmIcon
  , title: "Elm"
  , url: "https://elm-lang.org"
  , content: "elm-modal"
  , quote:
      Just
        { text: "A delightful language for reliable webapps."
        , source:
            USource
              { url: "https://elm.org"
              , name: "elm.org"
              }
        }
  }

c :: SkillDescription
c =
  { icon: A.cIcon
  , title: "C"
  , url: "https://en.wikipedia.org/wiki/C_(programming_language)"
  , content: "c-modal"
  , quote: Nothing
  }

cpp :: SkillDescription
cpp =
  { icon: A.cppIcon
  , title: "C++"
  , url: "https://en.wikipedia.org/wiki/C%2B%2B"
  , content: "cpp-modal"
  , quote: Nothing
  }

haskell :: SkillDescription
haskell =
  { icon: A.haskellIcon
  , title: "Haskell"
  , url: "https://www.haskell.org/"
  , content: "haskell-modal"
  , quote:
      Just
        { text: "An advanced, purely functional programming language"
        , source:
            USource
              { url: "https://haskell.org"
              , name: "haskell.org"
              }
        }
  }

lua :: SkillDescription
lua =
  { icon: A.luaIcon
  , title: "LUA"
  , url: "https://https://www.lua.org/"
  , content: "lua-modal"
  , quote: Nothing
  }

javascript :: SkillDescription
javascript =
  { icon: A.javascriptIcon
  , title: "Javascript"
  , url: "https://en.wikipedia.org/wiki/JavaScript"
  , content: "javascript-modal"
  , quote: Nothing
  }

csharp :: SkillDescription
csharp =
  { icon: A.csharpIcon
  , title: "C#"
  , url: "https://en.wikipedia.org/wiki/C_Sharp_(programming_language)"
  , content: "csharp-modal"
  , quote: Nothing
  }

coffeescript :: SkillDescription
coffeescript =
  { icon: A.coffeescriptIcon
  , title: "CoffeeScript"
  , url: "https://coffeescript.org/"
  , content: "coffeescript-modal"
  , quote: Nothing
  }

ruby :: SkillDescription
ruby =
  { icon: A.rubyIcon
  , title: "Ruby"
  , url: "https://www.ruby-lang.org/en/"
  , content: "ruby-modal"
  , quote: Nothing
  }

python :: SkillDescription
python =
  { icon: A.pythonIcon
  , title: "Python"
  , url: "https://www.python.org/"
  , content: "python-modal"
  , quote: Nothing
  }

html :: SkillDescription
html =
  { icon: A.htmlIcon
  , title: "HTML"
  , url: "https://en.wikipedia.org/wiki/HTML"
  , content: "html-modal"
  , quote: Nothing
  }

css :: SkillDescription
css =
  { icon: A.cssIcon
  , title: "CSS"
  , url: "https://en.wikipedia.org/wiki/Cascading_Style_Sheets"
  , content: "css-modal"
  , quote: Nothing
  }

rust :: SkillDescription
rust =
  { icon: A.rustIcon
  , title: "Rust"
  , url: "https://www.rust-lang.org/"
  , quote:
      Just
        { text: "A language empowering everyone to build reliable and efficient software."
        , source: USource { url: "https://www.rust-lang.org/", name: "rust-lang.org" }
        }
  , content: "rust-modal"
  }

technicalSkillL = localize "technical-skills" :: Localizer

otherTechnicalSkillL = localize "other-technical-skills" :: Localizer

programingLanguagesL = localize "programing-languages" :: Localizer

otherSkillDescL = localize "other-skills-description" :: Localizer

cite :: forall w i. Source -> Array (HH.HTML w i)
cite (USource source) = [ HH.cite [ HP.title source.name ] [ HH.a [ HP.href source.url, HP.target "_blank" ] [ HH.text source.name ] ] ]

cite (Source source) = [ HH.cite [ HP.title source ] [ HH.text source ] ]

technicalSkills :: forall a m r. MonadEffect m => State -> HH.HTML (H.ComponentSlot HH.HTML (ChildSlots r) m a) a
technicalSkills model =
  let
    st = do
      CSS.justifyContent CSS.spaceAround
      CSS.paddingTop (CSS.px 10.0)
      CSS.paddingBottom (CSS.px 10.0)
  in
    category "skills" (technicalSkillL model)
      [ subcategory "progLanguage" (programingLanguagesL model)
          [ HH.div [ HC.style st ]
              $ map (\(Tuple s i) -> mkSkillLink s i)
                  [ (Tuple "purescript" purescript)
                  , (Tuple "elm" elm)
                  , (Tuple "cpp" cpp)
                  , (Tuple "haskell" haskell)
                  , (Tuple "c" c)
                  , (Tuple "lua" lua)
                  , (Tuple "js" javascript)
                  , (Tuple "csharp" csharp)
                  , (Tuple "coffeescript" coffeescript)
                  , (Tuple "ruby" ruby)
                  , (Tuple "python" python)
                  , (Tuple "html" html)
                  , (Tuple "css" css)
                  , (Tuple "rust" rust)
                  ]
          ]
      , subcategoryHidden "progTech" (otherTechnicalSkillL model)
          [ para (otherSkillDescL model)
          , HH.ul [ HP.classes [ BS.listInline ] ]
              $ map
                  ( \s ->
                      HH.li [ HP.classes [ BS.listInlineItem ] ]
                        [ HH.span [ HP.classes [ BS.badge, BS.badgeSecondary ] ] [ HH.text s ] ]
                  )
                  [ "Git"
                  , "SQL (PostGreSQL)"
                  , "NoSQL DBMS (MongoDB)"
                  , "Linux CLI"
                  , "Clang toolchain"
                  , "gdb"
                  , "CMake/make"
                  , "dotTrace/dotMemory"
                  , ".NET"
                  , "Qt"
                  , "Boost"
                  , "WPF"
                  , "Node.js"
                  , "React"
                  , "Vuejs"
                  , "Django"
                  , "Ruby on Rails"
                  , "JIRA"
                  ]
          ]
      ]
  where
  quote :: String -> Maybe Quote -> String -> Array (HH.HTML (H.ComponentSlot HH.HTML (ChildSlots r) m a) a)
  quote id Nothing content = [ HH.slot _modalContent id M.component { text: localize content model, id: content } absurd ]

  quote id (Just qu) content =
    [ format qu
    , HH.slot _modalContent id M.component { text: localize content model, id: content } absurd
    ]

  format :: forall w j. Quote -> HH.HTML w j
  format q =
    HH.blockquote
      [ HP.classes [ BS.blockquote, BS.textRight ] ]
      [ para q.text
      , HH.footer [ HP.class_ BS.blockquoteFooter ] $ cite q.source
      ]

  mkSkillLink :: String -> SkillDescription -> HH.HTML (H.ComponentSlot HH.HTML (ChildSlots r) m a) a
  mkSkillLink id desc =
    HH.div
      [ HP.class_ BS.card, HC.style (CSS.display CSS.inlineBlock) ]
      [ modal id
          ( \a ->
              HH.button
                (a `snoc` HP.title desc.title `snoc` HP.class_ (HH.ClassName "skillButton"))
                [ A.iconS desc.icon [ HP.class_ (HH.ClassName "skillIcon") ]
                ]
          )
          ( \a ->
              [ A.icon desc.icon 5.0
              , HH.h3
                  (a `snoc` HC.style (CSS.margin auto auto auto auto))
                  [ HH.a [ HP.href desc.url ] [ HH.text desc.title ] ]
              ]
          )
          (quote id desc.quote desc.content)
          model
      ]
