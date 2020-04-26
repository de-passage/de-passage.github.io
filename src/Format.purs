module Format (para, h1, h2, h3, h4, h5, h6) where 

import Halogen.HTML as HH

para :: forall w i. String -> HH.HTML w i
para t =
    HH.p_ [ HH.text t ]

h1 :: forall w i. String -> HH.HTML w i
h1 t = 
    HH.h1_ [ HH.text t ]

h2 :: forall w i. String -> HH.HTML w i
h2 t = 
    HH.h2_ [ HH.text t ]

h3 :: forall w i. String -> HH.HTML w i
h3 t = 
    HH.h3_ [ HH.text t ]

h4 :: forall w i. String -> HH.HTML w i
h4 t = 
    HH.h4_ [ HH.text t ]

h5 :: forall w i. String -> HH.HTML w i
h5 t = 
    HH.h5_ [ HH.text t ]

h6 :: forall w i. String -> HH.HTML w i
h6 t = 
    HH.h6_ [ HH.text t ]


