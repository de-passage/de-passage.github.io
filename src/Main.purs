module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Resume as R

main :: Effect Unit
main = HA.runHalogenAff do 
  body <- HA.awaitBody
  runUI R.component unit body