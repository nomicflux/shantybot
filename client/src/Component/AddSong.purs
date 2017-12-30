module Component.AddSong where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (encodeJson, toObject)
import Data.Array (init, range, snoc, updateAt)
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
import Song.Song (mkSong)


type State = { title :: String
             , chorus :: String
             , verses :: Array String
             , numVerses :: Int
             }

initialState :: State
initialState = { title: ""
               , chorus: ""
               , verses: []
               , numVerses: 0
               }

data Query a = SetTitle String a
             | SetChorus String a
             | AddVerse a
             | DeleteVerse a
             | SetVerse Int String a
             | SendSong a

addVerse :: State -> State
addVerse state =
  let newNum = state.numVerses + 1
      newVerses = snoc state.verses ""
  in state { numVerses = newNum, verses = newVerses }

deleteVerse :: State -> State
deleteVerse state =
  let newNum = state.numVerses - 1
      newVerses = fromMaybe [] $ init state.verses
  in state { numVerses = if newNum > 0 then newNum else 0
           , verses = newVerses
           }

updateVerses :: Int -> String -> State -> State
updateVerses num verse state = case updateAt num verse state.verses of
  Just res -> state { verses = res }
  Nothing -> state

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AJAX | eff))
eval (SetTitle title next) =
  H.modify (_ { title = title }) *> pure next
eval (SetChorus chorus next) =
  H.modify (_ { chorus = chorus }) *>  pure next
eval (AddVerse next) =
  H.modify addVerse *> pure next
eval (DeleteVerse next) =
  H.modify deleteVerse *> pure next
eval (SetVerse num verse next) =
  H.modify (updateVerses num verse) *> pure next
eval (SendSong next) = do
  title <- H.gets _.title
  chorus <- H.gets _.chorus
  verses <- H.gets _.verses
  let song = mkSong title chorus verses
      req = AX.defaultRequest { url = "http://127.0.0.1:8803/add"
                              , method = Left POST
                              , content = Just $ encodeJson song
                              , headers = [ Accept $ MediaType "application/json"
                                          , ContentType $ MediaType "application/json"
                                          ]
                              }
  response <- H.liftAff $ AX.affjax req
  let newText = toObject response.response
  H.put initialState
  pure next

addDeleteDiv :: H.ComponentHTML Query
addDeleteDiv =
  HH.div [ HP.class_ (HH.ClassName "pure-u-1-6") ]
         [ HH.button [ HP.class_ (HH.ClassName "pure-button")
                     , HE.onClick (HE.input_ AddVerse) ] [ HH.text "Add Verse" ]
         , HH.button [ HP.class_ (HH.ClassName "pure-button")
                     , HE.onClick (HE.input_ DeleteVerse) ] [ HH.text "Remove Verse"]
         , HH.button [ HP.class_ (HH.ClassName "pure-button")
                     , HE.onClick (HE.input_ SendSong) ] [ HH.text "Record Song"]
         ]

titleDiv :: State -> H.ComponentHTML Query
titleDiv state =
  HH.div [ HP.class_ (HH.ClassName "pure-u-1-6") ]
         [ HH.label [ HP.for "title"] [HH.text "Title: " ]
         , HH.input [ HP.name "title"
                    , HP.value state.title
                    , HE.onValueInput (HE.input SetTitle)
                    ]
         ]

chorusDiv :: State -> H.ComponentHTML Query
chorusDiv state =
  HH.div [ HP.class_ (HH.ClassName "pure-u-2-6") ]
         [ HH.label [ HP.for "chorus"] [HH.text "Chorus: " ]
         , HH.textarea [ HP.name "chorus"
                       , HP.value state.chorus
                       , HE.onValueInput (HE.input SetChorus)
                       ]
                   ]

verseDiv :: Int -> H.ComponentHTML Query
verseDiv num = HH.div_ [ HH.label [ HP.for ("verse" <> show num) ]
                                  [ HH.text $ "Verse " <> show num <> " : " ]
                       , HH.textarea [ HP.name "verse"
                                     , HE.onValueInput (HE.input $ SetVerse (num - 1))
                                     ]
                       ]

versesDiv :: State -> H.ComponentHTML Query
versesDiv state =
  HH.div [ HP.class_ (HH.ClassName "pure-u-2-6") ]
         (if state.numVerses > 0 then verseDiv <$> range 1 state.numVerses else [])

render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ (HH.ClassName "pure-g pure-u-1") ]
    [ HH.div [ HP.classes (HH.ClassName <$> ["pure-u-1", "pure-form", "pure-form-aligned"]) ]
             [ titleDiv state
             , chorusDiv state
             , versesDiv state
             , addDeleteDiv
             ]
    ]

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AJAX | eff))
component = H.component { initialState: const initialState
                        , render
                        , eval
                        , receiver: const Nothing
                        }
