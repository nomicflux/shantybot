module Component.Dashboard where

import Prelude

import Component.MatchPhrase as MP
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

data Page = GetSong | AddSong | GetMatch

type State = { currentPage :: Page }

initialState :: State
initialState = { currentPage: GetMatch }

data Query a = ChangePage Page a

type ChildQuery = MP.Query
data Slot = MatchSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

eval :: forall m. Query ~> H.ParentDSL State Query ChildQuery Slot Void m
eval = case _ of
  ChangePage page next -> do
    H.modify $ _ { currentPage = page }
    pure next

pageToName :: Page -> String
pageToName GetSong = "Get Song by Title"
pageToName AddSong = "Add New Song"
pageToName GetMatch = "Find Song by Phrase"

pages :: Array Page
pages  = [GetSong, AddSong, GetMatch]

menuItem :: forall m. Page -> H.ParentHTML Query ChildQuery Slot m
menuItem page =
  HH.li [ HP.class_ $ HH.ClassName "pure-menu-item" ]
        [ HH.a [ HP.href "#"
               , HP.class_ $ HH.ClassName "pure-menu-link"
               , HE.onClick $ HE.input_ (ChangePage page) ]
               [ HH.text $ pageToName page ]
        ]

navbar :: forall m. H.ParentHTML Query ChildQuery Slot m
navbar =
  HH.nav [ HP.classes (HH.ClassName <$> ["pure-menu", "pure-menu-horizontal", "pure-u"]) ]
         [ HH.ul [ HP.class_ $ HH.ClassName "pure-menu-list" ]
                 ( menuItem <$> pages )
         ]

pageDiv :: forall m. State -> H.ParentHTML Query ChildQuery Slot (Aff (ajax :: AJAX | m))
pageDiv state = case state.currentPage of
  GetSong -> HH.div [ HP.class_ $ HH.ClassName "pure-u" ] [ HH.text "nothing yet" ]
  AddSong -> HH.div [ HP.class_ $ HH.ClassName "pure-u" ] [ HH.text "Nothing Yet" ]
  GetMatch -> HH.slot MatchSlot MP.component unit (const Nothing)

render :: forall m. State -> H.ParentHTML Query ChildQuery Slot (Aff (ajax :: AJAX | m))
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g"]
         [ navbar
         , pageDiv state
         ]

component :: forall m. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | m))
component =
  H.parentComponent { initialState: const initialState
                    , render
                    , eval
                    , receiver: const Nothing
                    }
