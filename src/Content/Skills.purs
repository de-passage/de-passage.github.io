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
  ) where

import Attributes

import Assets as A
import CSS as CSS
import CSS.Common (auto)
import Category (category)
import Data.Maybe as M
import Data.Tuple (Tuple(..))
import Format (para, h6)
import Halogen.HTML (p_, text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap4 as BS
import Prelude (($), map, (<>), negate)

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

type SkillDescription w i
  = { icon :: String
    , title :: String
    , content :: Array (HH.HTML w i)
    , url :: String
    , quote :: M.Maybe Quote
    }

projectTitle :: forall w i. HH.HTML w i
projectTitle = h6 "Projects"

purescript :: forall w i. SkillDescription w i
purescript =
  { icon: A.purescriptIcon
  , title: "Purescript"
  , url: "https://purescript.org"
  , content:
      [ para
          """ I started getting interested in Purescript because of its resemblance with Haskell and its focus
              on front-end development. I originally went with Elm, but the limitations of that language eventually
              brought me to seek a language with more expressive power. 
            """
      , para """It is a very interesting language, although the compiler error output is very hard to understand, 
          which slows down learning. My experience with the language is fairly limited but I expect to write a lot
          more code using it."""
      , projectTitle
      , p_
          [ text "This website is written using Purescript and Halogen."
          , HH.br_
          , HH.a [ HP.href "https://https://github.com/de-passage/de-passage.github.io" ] [ HH.text "Code" ]
          ]
      ]
  , quote:
      M.Just
        { text: "Purescript is a strongly-typed functional programming language that compiles to JavaScript."
        , source:
            USource
              { url: "https://purescript.org"
              , name: "purescript.org"
              }
        }
  }

elm :: forall w i. SkillDescription w i
elm =
  { icon: A.elmIcon
  , title: "Elm"
  , url: "https://elm-lang.org"
  , content:
      [ para
          """I started learning Elm as an alternative to Javascript for front-end development. I had already
          been playing with Haskell for a while, and I was sold to the idea that strongly typed languages 
          offered a massive advantage over loosely typed ones. Elm has a very well thought out set of 
          features and libraries, and getting started and learning the language is a pain-free experience.
          """
      , para
          """Unfortunately, the insistence of the language authors to limit the amount of highly abstract features
          (which is understandable considering the target audience) quickly got in my way. I am strongly in 
          favor of using high levels of abstraction to develop EDSLs representing the business domain of my 
          applications. This is simply not possible using Elm and was the motivation for trying out Purescript
          as an alternative.
          """
      , projectTitle
      , HH.p_
          [ HH.text "I wrote a few simple games using Elm as a way to get into the language."
          , HH.br_
          , HH.a [ HP.href "/front-end-games.elm/" ] [ HH.text "Live" ]
          , HH.br_
          , HH.a [ HP.href "https://github.com/de-passage/front-end-games.elm/" ] [ HH.text "Code" ]
          ]
      ]
  , quote:
      M.Just
        { text: "A delightful language for reliable webapps."
        , source:
            USource
              { url: "https://elm.org"
              , name: "elm.org"
              }
        }
  }

c :: forall w i. SkillDescription w i
c =
  { icon: A.cIcon
  , title: "C"
  , url: "https://en.wikipedia.org/wiki/C_(programming_language)"
  , content:
      [ para
          """C is an emblematic language and the first non proprietary language I learned (technically the first language
          I ever programmed in was TI Basic but not for a very long time). It is also a very old language and not 
          one I would ever use nor recommend for any real application anymore. Although it can be argued that the 
          its feature set involve very little cost compared to raw assembly languages, there is nothing it does that 
          cannot be done equally well in C++. It also lacks any form of support for type level programming and code
          generation (unless you really want to program with macros, which I wouldn't recommend). 
          """
      , para
          """I quickly transitionned to C++ and never looked back. The language itself is simple enough that I 
          am confident that I could pick it up and work with it again if needed.
          """
      , para "Since I haven't written any code in C in a long time, I currently do not have any project in C."
      ]
  , quote: M.Nothing
  }

cpp :: forall w i. SkillDescription w i
cpp =
  { icon: A.cppIcon
  , title: "C++"
  , url: "https://en.wikipedia.org/wiki/C%2B%2B"
  , content:
      [ para
          """C++ is the first language that I really enjoyed studying and still one of my favorite languages. I
      have been using it since around 2006 and like it's ability to define EDSLs and generate code through templates.
      It has fairly good support for type level programming and the capacity to produce both very low level, highly
      efficient, and very high level, very highly expressive code. However template metaprogramming is very verbose,
      and lately I've been prefering languages in the Haskell family for their terseness. Developping and deploying
      visually appealing projects in C++ can also be bothersome which fueled my interest for front-end development.
      """
      , projectTitle
      , para
          """Most of my personal projects deal with metaprogramming. It is the subset of C++ that I find the 
            most intellectually compelling.
          """
      , HH.ul_
          [ HH.li_
              [ HH.text
                  """Definition of some simple concepts of category theory in C++. The project was put on standby
                    because I couldn't decide of the best way to represent more complicated concepts within the confine of the
                    language. 
                  """
              , HH.a [ HP.href "https://github.com/de-passage/playing-with-category-theory.cpp" ] [ HH.text "Code" ]
              ]
          , HH.li_
              [ HH.text
                  """A defunct project for code generation through combination of tags. The idea was to create classes 
                with predefined behaviour, combine them, then customize parts of them.
              """
              , HH.a [ HP.href "https://github.com/de-passage/properties.cpp" ] [ HH.text "Code" ]
              ]
          , HH.li_
              [ HH.text
                  """Hana-style metaprograming project providing a way to create types with mathematical operators.
              """
              , HH.a [ HP.href "https://github.com/de-passage/algebraic-datatypes.cpp" ] [ HH.text "Code" ]
              ]
          ]
      ]
  , quote: M.Nothing
  }

haskell :: forall w i. SkillDescription w i
haskell =
  { icon: A.haskellIcon
  , title: "Haskell"
  , url: "https://www.haskell.org/"
  , content:
      [ para
          """I originally started Haskell because of its reputation as a difficult language, and one that pushes
              to rethink the way one writes software. And it is indeed difficult for anyone used to the imperative 
              paradigms. A lot of what I learned with Haskell now pervades my style in other languages as well.
            """
      , HH.p_
          [ HH.text
              """I have not yet gone to the extent of writing a full program in Haskell, although I did it with some
              Haskell derivatives like Elm. Most of the Haskell code I wrote was as part of my learning, using
            """
          , HH.a [ HP.href "https://codewars.com" ] [ HH.text "codewars.com" ]
          , HH.text " as a support."
          ]
      , projectTitle
      , HH.p_
          [ HH.text "I have a collection of disparate code fragments solving a variety of problems in Haskell."
          , HH.br_
          , HH.a [ HP.href "https://https://github.com/de-passage/codewars-solutions" ] [ HH.text "Code" ]
          ]
      ]
  , quote:
      M.Just
        { text: "An advanced, purely functional programming language"
        , source:
            USource
              { url: "https://haskell.org"
              , name: "haskell.org"
              }
        }
  }

lua :: forall w i. SkillDescription w i
lua =
  { icon: A.luaIcon
  , title: "LUA"
  , url: "https://https://www.lua.org/"
  , content:
      [ para
          """LUA is an interpreted language with good performance characteristics and a clean C API.
          I learned LUA shortly after C++, and integrating it in C++ applications taught me a lot about 
          the inner workings of interpreted languages and the difference between interpretation and compilation.
          """
      , para
          """LUA has a fairly unique metatable concept that was also my first introduction to metaprogramming.
          It is unfortunately fairly verbose and dynamically typed, two characteristics that have detered me from
          taking on new projects using it. I would still consider it as a good choice as a scripting language in
          a larger project with strict performance requirements."""
      ]
  , quote: M.Nothing
  }

javascript :: forall w i. SkillDescription w i
javascript =
  { icon: A.javascriptIcon
  , title: "Javascript"
  , url: "https://en.wikipedia.org/wiki/JavaScript"
  , content:
      [ para
          """I have a strongly negative opinion of Javascript. It is a very poorly thought out language and
        wholly unsuited for commercial use. It simply has too many flaws for any serious project, and with the 
        number of alternative languages transpiling to Javascript, I do not see any reason to keep using it. """
      , para
          """It is still an important language to know in the front end development ecosystem and I have written
      enough code using it (and Coffeescript, which is just Javascript with a nicer syntax) to be comfortable with
      its concepts and aware of the multiple issues that plague development using JS."""
      , para
          """If given a choice, I would rather use a language with a very strong type system like Elm or Purescript,
          or, if under very strict performance requirements, Coffeescript."""
      ]
  , quote: M.Nothing
  }

csharp :: forall w i. SkillDescription w i
csharp =
  { icon: A.csharpIcon
  , title: "C#"
  , url: "https://en.wikipedia.org/wiki/C_Sharp_(programming_language)"
  , content: 
    [ para """I had to learn C# as part of my job at Nexter Systems, as most of the project was using C# and the 
        .NET framework."""
    , para """C# is a decent language for enterprise use, as it is fairly easy to use for the average programmer,
        but I find it completely uninspiring. Type level programming is nigh impossible and metaprogramming involves
        runtime reflexion, which is essentially throwing the benefits of the type system out the window, not to 
        mention the performance penalty. The language also imposes the use of heap allocation and nullable 
        references for most types, which causes an endless stream of runtime crashes due to invalid references."""
    , para """The best part of C# is, in my opinion, the .NET framework rather than the language itself. It is 
      a viable option for Windows specific development in a setting with a large team of people at various skill
      levels, but I wouldn't use it for any personal project.""" ]
  , quote: M.Nothing
  }

coffeescript :: forall w i. SkillDescription w i
coffeescript =
  { icon: A.coffeescriptIcon
  , title: "CoffeeScript"
  , url: "https://coffeescript.org/"
  , content: 
    [ para """Coffeescript is a small language transpiling to Javascript. It is very interesting in that it is 
        "just Javascript" with a clean syntax. It adds a few syntactic sugar and hides some of the pitfalls of
        Javascript without abstracting it too much. It is usually very straightfoward to understand what the
        generated Javascript will be, by just looking at the Coffeescript code."""
      , para """However this also means that Coffeescript shares the inherent weaknesses of Javascript: lack 
        of typing and lack of scoping. It also has fallen out of fashion with the advent of newer versions of
        Javascript which included much of the syntactic sugar pioneered by Coffeescript."""
      , para """If I had to work with a derivative of Javascript that allows low level control over the generated
        code, Coffeescript is still a decent option in my opinion, but I would probably give Typescript or another
        typed variety of Javascript a try."""
      , projectTitle
      , HH.p_ 
        [ HH.text """I have a fair number of projects using Coffeescript on my Github, but I fell that most of them
          aren't very interesting.""" 
        , HH.br_
        , HH.text "Here is an implementation of Conway's Game of life. "
        , HH.a [ HP.href "https://github.com/de-passage/game-of-life.coffee" ] [ HH.text "Code " ]
        , HH.a [ HP.href "https://codepen.io/de-passage/full/WjWExY" ] [ HH.text "Live "]
        , HH.a [ HP.href "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life" ] [ HH.text "Wikipedia"]
        ]
    ]
  , quote: M.Nothing
  }

ruby :: forall w i. SkillDescription w i
ruby =
  { icon: A.rubyIcon
  , title: "Ruby"
  , url: "https://www.ruby-lang.org/en/"
  , content: 
    [ para """Ruby is by far my favorite scripting language. It has a very elegant syntax and allows for easy
      metaprogramming. Its performance characteristics are poor, but I mostly use it for lightweight, single use
      code. I got started with it with Ruby on Rails, although I don't have any public project using it anymore."""
    , para """My main complaint against Ruby is the dynamic type system that often results in runtime errors due
      to type mismatches. It is well suited as a scripting language for short programs and automating tedious tasks,
      but I wouldn't consider it for serious production use.""" ]
  , quote: M.Nothing
  }

python :: forall w i. SkillDescription w i
python =
  { icon: A.pythonIcon
  , title: "Python"
  , url: "https://www.python.org/"
  , content: 
    [ para """Most of my knowledge of Python actually comes from translating my experience with Ruby to this 
        language. Python is very popular in the space of interpreted, dynamically typed languages, but I believe
        that it suffers from the same kind of problems as Ruby, namely poor performances and the lack of a strong
        type system. Even though Python performances are better than Ruby, they are nowhere near that of compiled
        languages."""
    , para """I also find its syntax less clean than that of Ruby and the strict formatting annoying."""
    , projectTitle
    , para """I wrote a small website in Python using Django as a framework, but the code is not public as of now."""
    ]
  , quote: M.Nothing
  }

html :: forall w i. SkillDescription w i
html =
  { icon: A.htmlIcon
  , title: "HTML"
  , url: "https://en.wikipedia.org/wiki/HTML"
  , content: 
    [ para """HTML is not really a programming language, it's a markup language. I list it here because I don't 
        want someone to mistakenly assume that I don't know anything about it because it is not listed here, but 
        there is really not much to know about it."""
    , projectTitle
    , para "All the front end projects I've written use HTML either directly or through some sort of preprocessing."]
  , quote: M.Nothing
  }

css :: forall w i. SkillDescription w i
css =
  { icon: A.cssIcon
  , title: "CSS"
  , url: "https://en.wikipedia.org/wiki/Cascading_Style_Sheets"
  , content: 
    [ para """CSS is a stylesheet language used to describe the presentation of a document, notably HTML. It is
        interesting that the inherent flaws of the language and its implementations make it a technology that needs
        proper training rather than a straightforward tool like HTML. Writing CSS is not something I enjoy, although
        development using it has been considerably eased with realtime in-browser editing and visualization.
      """
    , para """A lot of languages transpiling to CSS (SASS, LESS...) have emerged to try to mitigate the pain of 
        CSS development, but no matter the level of abstraction, I can't help but find using CSS and its derivative 
        tedious and boring. I usually use Bootstrap as a framework and limit my work on the visual side to the
        strict minimum."""
    , projectTitle
    , para "All my front end projects use CSS under one form or another."
    ]
  , quote: M.Nothing
  }

cite :: forall w i. Source -> Array (HH.HTML w i)
cite (USource source) = [ HH.cite [ HP.title source.name ] [ HH.a [ HP.href source.url ] [ HH.text source.name ] ] ]

cite (Source source) = [ HH.cite [ HP.title source ] [ HH.text source ] ]

quote :: forall w i. M.Maybe Quote -> Array (HH.HTML w i) -> Array (HH.HTML w i)
quote M.Nothing content = [ HH.div_ content ]

quote (M.Just qu) content =
  [ format qu
  , HH.div_ content
  ]
  where
  format :: forall r j. Quote -> HH.HTML r j
  format q =
    HH.blockquote
      [ HP.classes [ BS.blockquote, BS.textRight ] ]
      [ para q.text
      , HH.footer [ HP.class_ BS.blockquoteFooter ] $ cite q.source
      ]

mkSkillLink :: forall w i. String -> SkillDescription w i -> HH.HTML w i
mkSkillLink id desc =
  HH.div
    [ HP.class_ BS.card, HC.style (CSS.display CSS.inlineBlock) ]
    [ HH.button
        [ dataToggle "modal"
        , dataTarget ("#modal" <> id)
        , HP.title desc.title
        ]
        [ A.iconS desc.icon [HP.class_ (HH.ClassName "skillIcon")]
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
        [ HH.div [ HP.class_ BS.modalDialog, HC.style (CSS.maxWidth (CSS.pct 90.0)) ]
            [ HH.div [ HP.class_ BS.modalContent ]
                [ HH.div [ HP.class_ BS.modalHeader ]
                    [ A.icon desc.icon 5.0
                    , HH.h3
                        [ HP.class_ BS.modalTitle
                        , HC.style (CSS.margin auto auto auto auto)
                        , HP.id_ ("backdropLabel" <> id)
                        ]
                        [ HH.a [ HP.href desc.url ] [ HH.text desc.title ] ]
                    ]
                , HH.div [ HP.classes [ BS.modalBody, BS.textJustify ] ] (quote desc.quote desc.content)
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
