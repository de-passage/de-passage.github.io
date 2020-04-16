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
  ) where

import Assets as A
import Halogen.HTML (div_, p_, text)
import Halogen.HTML as HH

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
