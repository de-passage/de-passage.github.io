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
  ) where

import Prelude (($), map, (<>), negate)
import Assets as A
import Attributes
import Category (category)
import CSS as CSS
import CSS.Common (auto)
import Data.Tuple (Tuple(..))
import Halogen.HTML (div_, p_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS

type SkillDescription w i
  = { icon :: A.Icon w i
    , title :: String
    , content :: HH.HTML w i
    , url :: String
    }

purescript :: forall w i. SkillDescription w i
purescript =
  { icon: A.purescriptIcon
  , title: "Purescript"
  , url: "https://purescript.org"
  , content:
      p_
        [ text "Purescript is a strongly-typed functional programming language that compiles to JavaScript."
        ]
  }

elm :: forall w i. SkillDescription w i
elm =
  { icon: A.elmIcon
  , title: "Elm"
  , url: "https://elm-lang.org"
  , content: div_ []
  }

c :: forall w i. SkillDescription w i
c =
  { icon: A.cIcon
  , title: "C"
  , url: "https://en.wikipedia.org/wiki/C_(programming_language)"
  , content: div_ []
  }

cpp :: forall w i. SkillDescription w i
cpp =
  { icon: A.cppIcon
  , title: "C++"
  , url: "https://en.wikipedia.org/wiki/C%2B%2B"
  , content: div_ []
  }

haskell :: forall w i. SkillDescription w i
haskell =
  { icon: A.haskellIcon
  , title: "Haskell"
  , url: "https://www.haskell.org/"
  , content: div_ []
  }

lua :: forall w i. SkillDescription w i
lua =
  { icon: A.luaIcon
  , title: "LUA"
  , url: "https://https://www.lua.org/"
  , content: div_ []
  }

javascript :: forall w i. SkillDescription w i
javascript =
  { icon: A.javascriptIcon
  , title: "Javascript"
  , url: "https://en.wikipedia.org/wiki/JavaScript"
  , content: div_ []
  }

csharp :: forall w i. SkillDescription w i
csharp =
  { icon: A.csharpIcon
  , title: "C#"
  , url: "https://en.wikipedia.org/wiki/C_Sharp_(programming_language)"
  , content: div_ []
  }

coffeescript :: forall w i. SkillDescription w i
coffeescript =
  { icon: A.coffeescriptIcon
  , title: "CoffeeScript"
  , url: "https://coffeescript.org/"
  , content: div_ []
  }

ruby :: forall w i. SkillDescription w i
ruby =
  { icon: A.rubyIcon
  , title: "Ruby"
  , url: "https://www.ruby-lang.org/en/"
  , content: div_ []
  }

python :: forall w i. SkillDescription w i
python =
  { icon: A.pythonIcon
  , title: "Python"
  , url: "https://www.python.org/"
  , content: div_ []
  }

html :: forall w i. SkillDescription w i
html =
  { icon: A.htmlIcon
  , title: "HTML"
  , url: "https://en.wikipedia.org/wiki/HTML"
  , content: div_ []
  }

css :: forall w i. SkillDescription w i
css =
  { icon: A.cssIcon
  , title: "CSS"
  , url: "https://en.wikipedia.org/wiki/Cascading_Style_Sheets"
  , content: div_ []
  }


mkSkillLink :: forall w i. String -> SkillDescription w i -> HH.HTML w i
mkSkillLink id desc =
  HH.div
    [ HP.class_ BS.card, HC.style (CSS.display CSS.inlineBlock) ]
    [ HH.button
        [ dataToggle "modal"
        , dataTarget ("#modal" <> id)
        , HP.title desc.title
        ]
        [ desc.icon 7.0
        ]
    , HH.div
        [ HP.classes [ BS.modal, BS.fade ]
        , HP.id_ ("modal" <> id)
        , HP.tabIndex (-1)
        , dataBackdrop "static"
        , ARIA.role "dialog"
        , ARIA.labelledBy ("backdropLabel" <> id)
        , ARIA.hidden "true"
        ]
        [ HH.div [ HP.class_ BS.modalDialog ]
            [ HH.div [ HP.class_ BS.modalContent ]
                [ HH.div [ HP.class_ BS.modalHeader ]
                    [ desc.icon 5.0
                    , HH.h3
                        [ HP.class_ BS.modalTitle
                        , HC.style (CSS.margin auto auto auto auto)
                        , HP.id_ ("backdropLabel" <> id)
                        ]
                        [ HH.text desc.title ]
                    ]
                , HH.div [ HP.class_ BS.modalBody ] [ desc.content ]
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

technicalSkills :: forall w i. HH.HTML w i
technicalSkills =
  category "skills" "Technical skills"
    [ HH.div [ HC.style (CSS.justifyContent CSS.spaceAround) ]
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
            ]
    ]