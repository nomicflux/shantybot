module Component.MatchSong where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Song.Song (Line(..), Song, getTitleText, getChorusText, getVersesText)

data Query a = SendPhrase a
             | SetPhrase String a

type State = { phrase :: Line
             , songs :: Array Song
             , loading :: Boolean
             }

initialState :: State
initialState = { phrase: Line "", songs: [], loading: false }

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
eval = case _ of
  SetPhrase phrase next -> do
    H.modify (_ { phrase = Line phrase })
    pure next
  SendPhrase next -> do
    phrase <- H.gets _.phrase
    H.modify (_ { loading = true })
    let
      req = AX.defaultRequest { url = "http://127.0.0.1:8803/match"
                              , method = Left POST
                              , content = Just $ encodeJson phrase
                              , headers = [ Accept $ MediaType "application/json"
                                          , ContentType $ MediaType "application/json"
                                          ]
                              }
    response <- H.liftAff $ AX.affjax req
    _ <- case decodeJson response.response of
      Left err ->
        pure unit
      Right songs ->
        H.modify (_ { songs = songs, loading = false })
    pure next

songClass :: HH.ClassName
songClass = HH.ClassName "song-song"
titleClass :: HH.ClassName
titleClass = HH.ClassName "song-title"
chorusClass :: HH.ClassName
chorusClass = HH.ClassName "song-chorus"
verseClass :: HH.ClassName
verseClass = HH.ClassName "song-verse"

arrayToHTML :: forall p i. Array String -> Array (HH.HTML p i)
arrayToHTML arr = HH.text <$> arr >>= (\t -> [t, HH.br_])

verseDiv :: Array String -> H.ComponentHTML Query
verseDiv verse = HH.div [ HP.class_ verseClass ] (arrayToHTML verse)

songDiv :: Song -> H.ComponentHTML Query
songDiv song =
  HH.div [ HP.class_ songClass ] body
  where
    body =
      catMaybes [ Just $ HH.div [HP.class_ titleClass] [ HH.span_ [ (HH.text $ getTitleText song) ] ]
                , (HH.div [HP.class_ chorusClass] <<< arrayToHTML) <$> getChorusText song
                , Just $ HH.div_ (verseDiv <$> getVersesText song)
                ]

songsDiv :: State -> H.ComponentHTML Query
songsDiv state =
  HH.div [ HP.class_ (HH.ClassName "pure-u-2-3") ]
         if state.loading then
           [ HH.text "Loading ..." ]
         else
           (songDiv <$> state.songs)

render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ (HH.ClassName "pure-g pure-u-1") ]
    [ HH.div [ HP.class_ (HH.ClassName "pure-u-1-3") ]
             [ HH.div [ HP.classes ( HH.ClassName <$> ["pure-form", "pure-form-aligned" ] )
                      ]
                      [ HH.div [ HP.class_ (HH.ClassName "pure-control-group") ]
                               [ HH.label [ HP.for "phrase" ] [ HH.text "Phrase to match: " ]
                               , HH.input [ HP.name "phrase"
                                          , HE.onValueInput (HE.input SetPhrase) ]
                               ]
                      , HH.button [ HP.class_ (HH.ClassName "pure-button")
                                  , HE.onClick (HE.input_ SendPhrase)
                                  ] [ HH.text "Get Match" ]
                      ] ]
    , songsDiv state
    ]

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
component = H.component { initialState: const initialState
                        , render
                        , eval
                        , receiver: const Nothing
                        }
