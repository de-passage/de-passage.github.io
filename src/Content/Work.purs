module Work where

import Prelude

import Attributes (scopeRow, scopeCol)
import CSS as CSS
import CSS.Overflow as CSS.Overflow
import Category (categoryHidden, subcategoryHidden)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
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
          [ HH.text "Although I was technically employed by "
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
    [ HH.div [ HP.class_ (HH.ClassName "work-experience") ] $ map mkExperience [ nexter ]
    , subcategoryHidden "otherwork" "Other work experiences"
        [ HH.p [ HP.classes [ BS.textLeft, BS.p2 ] ]
            [ HH.text
                """From 2011 to 2017 I occupied a number of unqualified positions around the world.
                Most of them were short term, sometimes volunteering, and in general irrelevant to my current 
                situation."""
            ]
        , HH.div
            [ HC.style (CSS.Overflow.overflow CSS.Overflow.overflowAuto *> CSS.maxHeight (CSS.vh 60.0)) ]
            [ HH.table [ HP.class_ BS.table ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th [ scopeCol ] [ HH.text "Year" ]
                        , HH.th [ scopeCol ] [ HH.text "Duration" ]
                        , HH.th [ scopeCol ] [ HH.text "Job" ]  
                        , HH.th [ scopeCol ] [ HH.text "Location" ]
                        ]
                    ]
                , HH.tbody_ $ map mkRow jobs
                ]
            ]
        ]
    ]

mkExperience :: forall w i. Experience w i -> HH.HTML w i
mkExperience exp =
  HH.div [ HP.classes [ BS.textJustify, BS.p2 ] ]
    [ HH.div
        [ HP.classes [ BS.row, BS.mb2 ] ]
        [ HH.h3
            [ HP.classes [ BS.col, BS.mtAuto ] ]
            [ HH.a
                [ HP.href exp.url ]
                [ HH.text exp.company ]
            ]
        , HH.h5 [ HP.classes [ BS.col, BS.mtAuto ] ] [ HH.text exp.job ]
        ]
    , HH.h5 [ HP.class_ BS.mb2 ] [ HH.text exp.period ]
    , HH.div_ exp.description
    ]
  
mkRow :: forall w i. Job -> HH.HTML w i
mkRow job =
  HH.tr_
    [ HH.th [ scopeRow ] [ HH.text job.year ]
    , HH.td_ [ HH.text job.duration ]
    , HH.td_ [ HH.text job.job ]
    , HH.td_ [ HH.text job.location ]
    ]

type Job
  = { job :: String
    , year :: String
    , location :: String
    , duration :: String
    }

jobs :: Array Job
jobs =
  [ { year: "2017 - 2018"
    , duration: "6 months"
    , job: "Farm hand"
    , location: "Various places in Australia"
    }
  , { year: "2017"
    , duration: "2 months"
    , job: "Factory hand"
    , location: "Victoria, Australia"
    }
  , { year: "2016"
    , duration: "4 months"
    , job: "Hostel employee"
    , location: "Several places in Taiwan"
    }
  , { year: "2016"
    , duration: "2 months"
    , job: "Hotel receptionist"
    , location: "Tokyo, Japan"
    }
  , { year: "2015"
    , duration: "4 months"
    , job: "Kitchen hand"
    , location: "Busan, South Korea"
    }
  , { year: "2015"
    , duration: "1 months"
    , job: "English conversation assistant"
    , location: "Busan, South Korea"
    }
  , { year: "2013 - 2014"
    , duration: "1 year"
    , job: "Shop clerk"
    , location: "Tokyo, Japan"
    }
  , { year: "2013"
    , duration: "4 months"
    , job: "Hostel employee"
    , location: "Tokyo, Japan"
    }
  , { year: "2012"
    , duration: "3 months"
    , job: "Shop clerk"
    , location: "France"
    }
  ]
