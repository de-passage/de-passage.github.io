module Attributes where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

dataToggleAN :: H.AttrName
dataToggleAN = H.AttrName "data-toggle"

dataTargetAN :: H.AttrName
dataTargetAN = H.AttrName "data-target"

dataBackdropAN :: H.AttrName
dataBackdropAN = H.AttrName "data-backdrop"

dataDismissAN :: H.AttrName
dataDismissAN = H.AttrName "data-dismiss"

dataToggle :: forall w i. String -> HP.IProp w i
dataToggle = HP.attr dataToggleAN

dataTarget :: forall w i. String -> HP.IProp w i
dataTarget = HP.attr dataTargetAN

dataBackdrop :: forall w i. String -> HP.IProp w i
dataBackdrop = HP.attr dataBackdropAN

dataDismiss :: forall w i. String -> HP.IProp w i
dataDismiss = HP.attr dataDismissAN

collapseAttr :: forall p i. HP.IProp p i
collapseAttr = dataToggle "collapse"

scope :: forall r i. String -> HH.IProp r i
scope = HH.attr (HH.AttrName "scope")

scopeRow :: forall r i. HH.IProp r i
scopeRow = scope "row"

scopeCol :: forall r i. HH.IProp r i
scopeCol = scope "col"