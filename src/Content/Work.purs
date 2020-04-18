module Work where

import Category (categoryHidden)
import Halogen.HTML as HH

workExperience :: forall w i. HH.HTML w i
workExperience = categoryHidden "work" "Work Experience" 
    [ HH.h3_ [ HH.text "" ]
    ]