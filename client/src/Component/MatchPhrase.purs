module Component.MatchPhrase where

import Prelude

import Control.Monad.Aff (Aff)
import Debug.Trace (spy)
import Data.Argonaut (class EncodeJson, encodeJson, jsonSingletonObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))

data Query a = SendPhrase a
             | SetPhrase String a

data Phrase = Phrase String

instance encodePhrase :: EncodeJson Phrase where
  encodeJson (Phrase phrase) = jsonSingletonObject "phrase" (encodeJson phrase)

type State = { phrase :: Phrase
             , text :: Maybe String
             , loading :: Boolean
             }

initialState :: State
initialState = { phrase: Phrase "", text: Nothing, loading: false }

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
eval = case _ of
  SetPhrase phrase next -> do
    H.modify (_ { phrase = Phrase phrase })
    pure next
  SendPhrase next -> do
    phrase <- H.gets _.phrase
    H.modify (_ { loading = true })
    let
      body = spy (encodeJson phrase)
      (Phrase text) = phrase
      req = spy (AX.defaultRequest { url = "http://127.0.0.1:8803/match/" <> text
                                   , method = Left GET
                                   , headers = [ Accept $ MediaType "application/json"
                                               , ContentType $ MediaType "application/json"
                                               ]
                                   })
    response <- H.liftAff $ AX.affjax req
    let newText = spy response.response
    H.modify (_ { text = Just newText, loading = false })
    pure next

songDiv :: State -> H.ComponentHTML Query
songDiv state =
  HH.div [ HP.class_ (HH.ClassName "pure-u-2-3") ]
         if state.loading then
           [ HH.text "Loading ..." ]
         else
           [ HH.text (fromMaybe "" state.text) ]


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
    , songDiv state
    ]

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
component = H.component { initialState: const initialState
                        , render
                        , eval
                        , receiver: const Nothing
                        }
