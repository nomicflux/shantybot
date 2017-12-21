module Component.MatchPhrase where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug.Trace (spy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX

data Query a = SendPhrase a
             | SetPhrase String a

type State = { phrase :: String
             , text :: Maybe String
             , loading :: Boolean
             }

initialState :: State
initialState = { phrase: "", text: Nothing, loading: false }

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
eval = case _ of
  SetPhrase phrase next -> do
    H.modify (_ { phrase = phrase })
    pure next
  SendPhrase next -> do
    phrase <- H.gets _.phrase
    H.modify (_ { loading = true })
    response <- H.liftAff $ AX.post "http://127.0.0.1:8803/match/" phrase
    let _ = spy response
    H.modify (_ { text = Just response.response, loading = false })
    pure next

render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ (HH.ClassName "pure-g") ]
    [ HH.div [ HP.class_ (HH.ClassName "pure-u-1-3") ]
             [ HH.form [ HP.classes ( HH.ClassName <$> ["pure-form", "pure-form-aligned" ] ) ]
                       [ HH.div [ HP.class_ (HH.ClassName "pure-control-group") ]
                                [ HH.label [ HP.for "phrase" ] [ HH.text "Phrase to match: " ]
                                , HH.input [ HE.onValueInput (HE.input SetPhrase) ]
                                ]
                       , HH.button [ HP.class_ (HH.ClassName "pure-button")
                                   , HE.onClick (HE.input_ SendPhrase)
                                   ] [ HH.text "Get Match" ]
                       ] ]
    , HH.div [ HP.class_ (HH.ClassName "pure-u-2-3") ]
             [ HH.text (fromMaybe "" state.text) ]
    ]

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
component = H.component { initialState: const initialState
                        , render
                        , eval
                        , receiver: const Nothing
                        }
