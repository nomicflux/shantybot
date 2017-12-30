module Component.Dashboard where

import Prelude

import Component.GetSong as GS
import Component.AddSong as AS
import Component.MatchSong as MS
import Control.Monad.Aff (Aff)
import Data.Array ((:))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

data Page = GetSong | AddSong | MatchSong

type State = { currentPage :: Page }

initialState :: State
initialState = { currentPage: MatchSong }

data Query a = ChangePage Page a

type ChildQuery = Coproduct3 GS.Query AS.Query MS.Query
type Slot = Either3 Unit Unit Unit

eval :: forall m. Query ~> H.ParentDSL State Query ChildQuery Slot Void m
eval = case _ of
  ChangePage page next -> do
    H.modify $ _ { currentPage = page }
    pure next

pageToName :: Page -> String
pageToName GetSong = "Get Song by Title"
pageToName AddSong = "Add New Song"
pageToName MatchSong = "Find Song by Phrase"

pages :: Array Page
pages  = [GetSong, AddSong, MatchSong]

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
  HH.nav [ HP.classes (HH.ClassName <$> ["pure-menu", "pure-menu-horizontal", "pure-u-1"]) ]
         [ HH.ul [ HP.classes $ HH.ClassName <$> ["pure-menu-list", "pure-u-1"] ]
                 ( HH.span [HP.class_ $ HH.ClassName "navbar-title"]
                           [HH.text "ShantyBot Admin Page"] : (menuItem <$> pages) )
         ]

pageDiv :: forall m. State -> H.ParentHTML Query ChildQuery Slot (Aff (ajax :: AJAX | m))
pageDiv state = case state.currentPage of
  GetSong -> HH.div [ HP.class_ $ HH.ClassName "pure-u" ] [ HH.text "nothing yet" ]
  AddSong -> HH.slot' CP.cp2 unit AS.component unit absurd
  MatchSong -> HH.slot' CP.cp3 unit MS.component unit absurd

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
