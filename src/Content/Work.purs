module Work where

import Prelude
import Attributes (scopeRow, scopeCol)
import CSS as CSS
import CSS.Overflow as CSS.Overflow
import Category (categoryHidden, subcategoryHidden)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Marked as M
import State (State, Localizer, localize)

type ChildSlots r
  = ( companyDescription :: M.Slot Unit, companyName :: M.Slot Unit | r )

_nexterDescription = SProxy :: SProxy "companyDescription"

_companyName = SProxy :: SProxy "companyName"

type Experience
  = { job :: String
    , description :: String
    , period :: String
    , company :: String
    }

softwareEngineerS = "software-engineer" :: String

nexterPeriodS = "nexter-period" :: String

nexterCompanyS = "nexter" :: String

nexterDescriptionS = "nexter-description" :: String

workExperienceL = localize "work-experience" :: Localizer

otherWorkExperienceL = localize "other-work-experience" :: Localizer

otherDescriptionL = localize "other-work-description" :: Localizer

nexter :: Experience
nexter =
  { job: softwareEngineerS
  , period: nexterPeriodS
  , company: nexterCompanyS
  , description: nexterDescriptionS
  }

workExperience :: forall r m a. MonadEffect m => State -> HH.HTML (H.ComponentSlot HH.HTML (ChildSlots r) m a) a
workExperience model =
  categoryHidden "work" (workExperienceL model)
    [ HH.div [ HP.class_ (HH.ClassName "work-experience") ] $ map mkExperience [ nexter ]
    , subcategoryHidden "otherwork" (otherWorkExperienceL model)
        [ HH.p [ HP.classes [ BS.textLeft, BS.p2 ] ]
            [ HH.text (otherDescriptionL model)
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
  where
  mkExperience :: Experience -> HH.HTML (H.ComponentSlot HH.HTML (ChildSlots r) m a) a
  mkExperience exp =
    HH.div [ HP.classes [ BS.textJustify, BS.p2 ] ]
      [ HH.div
          [ HP.classes [ BS.row, BS.mb2 ] ]
          [ HH.h3
              [ HP.classes [ BS.col12, BS.colMd6, BS.mtAuto ] ]
              [ HH.slot _companyName unit M.component { text: localize exp.company model, id: exp.company } absurd
              ]
          , HH.h5 [ HP.classes [ BS.col12, BS.mtAuto, BS.colMd6 ] ] [ HH.text (localize exp.job model) ]
          ]
      , HH.h5 [ HP.class_ BS.mb2 ] [ HH.text (localize exp.period model) ]
      , HH.slot _nexterDescription unit M.component { text: localize exp.description model, id: exp.description } absurd
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
