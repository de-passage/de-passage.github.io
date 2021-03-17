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
import Internationalization (Language(..))
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
    , url :: Localizer
    }

softwareEngineerS = "software-engineer" :: String

nexterPeriodS = "nexter-period" :: String

nexterDescriptionS = "nexter-description" :: String

workExperienceL = localize "work-experience" :: Localizer

otherWorkExperienceL = localize "other-work-experience" :: Localizer

otherDescriptionL = localize "other-work-description" :: Localizer

yearL = localize "year" :: Localizer

durationL = localize "duration" :: Localizer

jobL = localize "job" :: Localizer

locationL = localize "location" :: Localizer

nexterUrl :: Localizer
nexterUrl model = case model.language of
  Fr -> "https://www.nexter-group.fr"
  _ -> "https://www.nexter-group.fr/en"

nexter :: Experience
nexter =
  { job: softwareEngineerS
  , period: nexterPeriodS
  , company: "Nexter Systems"
  , url: nexterUrl
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
                        [ HH.th [ scopeCol ] [ HH.text (yearL model) ]
                        , HH.th [ scopeCol ] [ HH.text (durationL model) ]
                        , HH.th [ scopeCol ] [ HH.text (jobL model) ]
                        , HH.th [ scopeCol ] [ HH.text (locationL model) ]
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
              [ HH.a [ HP.href (exp.url model), HP.target "_blank" ] [ HH.text exp.company ]
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
      , HH.td_ [ HH.text (localize job.duration model) ]
      , HH.td_ [ HH.text (localize job.job model) ]
      , HH.td_ [ HH.text (localize job.location model) ]
      ]

type Job
  = { job :: String
    , year :: String
    , location :: String
    , duration :: String
    }

duration1Months = "duration-1-months" :: String

duration2Months = "duration-2-months" :: String

duration3Months = "duration-3-months" :: String

duration4Months = "duration-4-months" :: String

duration6Months = "duration-6-months" :: String

duration1Year = "duration-1-year" :: String

locationAustraliaMisc = "various-australia" :: String

locationAustraliaVic = "victoria-australia" :: String

locationTaiwanMisc = "various-taiwan" :: String

locationJapanTokyo = "tokyo-japan" :: String

locationSKoreaBusan = "busan-korea" :: String

locationFrance = "mlb-france" :: String

jobFarmHand = "job-farm-hand" :: String

jobFactoryHand = "job-factory-hand" :: String

jobKitchenHand = "job-kitchen-hand" :: String

jobHostelEmployee = "job-hostel-employee" :: String

jobReceptionist = "job-receptionist" :: String

jobEnglishConversation = "job-english-conversation" :: String

jobShopClerk = "job-shop-clerk" :: String

jobs :: Array Job
jobs =
  [ { year: "2017 - 2018"
    , duration: duration6Months
    , job: jobFarmHand
    , location: locationAustraliaMisc
    }
  , { year: "2017"
    , duration: duration1Months
    , job: jobFactoryHand
    , location: locationAustraliaVic
    }
  , { year: "2016"
    , duration: duration4Months
    , job: jobHostelEmployee
    , location: locationTaiwanMisc
    }
  , { year: "2016"
    , duration: duration2Months
    , job: jobReceptionist
    , location: locationJapanTokyo
    }
  , { year: "2015"
    , duration: duration4Months
    , job: jobKitchenHand
    , location: locationSKoreaBusan
    }
  , { year: "2015"
    , duration: duration1Months
    , job: jobEnglishConversation
    , location: locationSKoreaBusan
    }
  , { year: "2013 - 2014"
    , duration: duration1Year
    , job: jobShopClerk
    , location: locationJapanTokyo
    }
  , { year: "2013"
    , duration: duration4Months
    , job: jobHostelEmployee
    , location: locationJapanTokyo
    }
  , { year: "2012"
    , duration: duration3Months
    , job: jobShopClerk
    , location: locationFrance
    }
  ]
