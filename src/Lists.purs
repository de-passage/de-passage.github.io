module Lists where 

import Prelude (($), (<>), (>>>), map)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Bootstrap as BS
import Data.Newtype
import DOM.HTML.Indexed (HTMLli)

newtype ListItem w i
  = ListItem (HH.HTML w i)

derive instance newtypeListItem :: Newtype (ListItem w i) _

listGroup :: forall w i. Array (ListItem w i) -> HH.HTML w i
listGroup = map unwrap >>> HH.ul [ HP.classes [ BS.listGroup, BS.listGroupFlush ] ]

listGroupC :: forall w i. Array H.ClassName -> Array (ListItem w i) -> HH.HTML w i
listGroupC classes = map unwrap >>> HH.ul [ HP.classes $ classes <> [ BS.listGroup, BS.listGroupFlush ] ]

type AH w i
  = Array (HH.HTML w i)

listItem :: forall w i. Array (HH.IProp HTMLli i) -> Array (HH.HTML w i) -> ListItem w i
listItem props = HH.li (props <> [ HP.class_ BS.listGroupItem ]) >>> ListItem

listItem_ :: forall w i. Array (HH.HTML w i) -> ListItem w i
listItem_ = listItem []
