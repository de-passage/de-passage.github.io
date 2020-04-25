module Work where

import Prelude
import Category (categoryHidden)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS

type Experience w i
  = { job :: String
    , description :: Array (HH.HTML w i)
    , period :: String
    , company :: String
    , url :: String
    }

nexter :: forall w i. Experience w i
nexter =
  { job: "Software engineer"
  , period: "April 2018 - December 2019"
  , company: "Nexter Systems"
  , url: "https://www.nexter-group.fr/en"
  , description:
      [ HH.p_
          [ HH.text "Also I was technically employed by "
          , HH.a [ HP.href "https://www.akka-technologies.com/" ]
              [ HH.text "AKKA technologies" ]
          , HH.text " I only went to the office a handful of times. Most of my time was spent working at "
          , HH.text "the customer's office, working on the customer's project under their management."
          ]
      , HH.p_
          [ HH.text "The project was about the development of a guidance and communication system for military armored vehicles. My responsibilities involved: "
          , HH.ul_
              [ HH.li_ [ HH.text "Design and implementation of a custom database synchronization system (C++)." ]
              , HH.li_ [ HH.text "Refactoring of a legacy code base to improve reliability and modularity (C#)." ]
              , HH.li_ [ HH.text "Implementation of business-critical features in a large application (C#)." ]
              , HH.li_ [ HH.text "Set up of unit testing frameworks and implementation of unit tests (C++ and C#)." ]
              , HH.li_ [ HH.text "Profiling and performance improvements (C#)." ]
              , HH.li_ [ HH.text "Implementation of tools to help the development and quality analysis process (C# and C++)." ]
              , HH.li_ [ HH.text "Analysis and documentation of legacy code bases (C++ and C#)." ]
              ]
          ]
      , HH.p_
          [ HH.text "Technical Environment:"
          , HH.ul_
              [ HH.li_ [ HH.strong_ [ HH.text "Languages: " ], HH.text "C++, C#" ]
              , HH.li_ [ HH.strong_ [ HH.text "Tools: " ], HH.text "JIRA, Git, WPF, NUnit, dotTrace/dotMemory, Microsoft CppUnitTestFramework" ]
              ]
          ]
      ]
  }

workExperience :: forall w i. HH.HTML w i
workExperience =
  categoryHidden "work" "Work Experience"
    $ map mkExperience [ nexter ]

mkExperience :: forall w i. Experience w i -> HH.HTML w i
mkExperience exp =
  HH.div [ HP.classes [ BS.textJustify ] ]
    [ HH.div 
        [ HP.classes [ BS.row ] ]
        [ HH.h3
            [ HP.classes [ BS.col, BS.mtAuto ] ]
            [ HH.a
                [ HP.href exp.url ]
                [ HH.text exp.company ]
            ]
        , HH.h4 [ HP.classes [ BS.col, BS.mtAuto ] ] [ HH.text exp.job ]
        ]
    , HH.h4_ [ HH.text exp.period ]
    , HH.div_ exp.description
    ]
