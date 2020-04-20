module Projects where

import Lists
import Category (categoryHidden)
import Data.Maybe
import Halogen.HTML as HH

type Project w i
  = { codeurl :: Maybe String
    , liveUrl :: Maybe String
    , name :: String
    , description :: HH.HTML w i
    }

projectDescriptions :: forall w i. Array (Project w i)
projectDescriptions =
  []

projects :: forall w i. HH.HTML w i
projects =
  categoryHidden "projects" "Projects"
    [ listGroup
        [ listItem_ []
        ]
    ]
