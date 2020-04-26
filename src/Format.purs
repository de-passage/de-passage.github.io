module Format (para) where 

import Halogen.HTML as HH

para :: forall w i. String -> HH.HTML w i
para t =
    HH.p_ [ HH.text t ]

