{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Song.Song where

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, ToJSON, toJSON, object, (.=))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import TextMining.Document (Document(..), mkDocument, Line(..))

newtype Verse = Verse { verseLines :: [Line] }

instance FromJSON Verse where
  parseJSON = withObject "VerseObject" $ \o ->
    Verse <$> o .: "verse"

instance ToJSON Verse where
  toJSON (Verse verse) = object [ "verse" .= verse ]

data Song = Song { songTitle :: Text
                 , songChorus :: [Line]
                 , songVerses :: [Verse]
                 }

instance FromJSON Song where
  parseJSON = withObject "SongObject" $ \o -> do
    songTitle <- o .: "title"
    songChorus <- o .: "chorus"
    songVerses <- o .: "verses"
    return Song{..}

instance ToJSON Song where
  toJSON Song{..} = object [ "title" .= songTitle
                           , "chorus" .= songChorus
                           , "verses" .= songVerses
                           ]

newtype SongDictionary = SongDictionary (Map Text [Song])

addToDictionary :: SongDictionary -> Song -> SongDictionary
addToDictionary (SongDictionary songDict) song =
  case M.lookup title songDict of
    Just current -> SongDictionary $ M.insert title (song : current) songDict
    Nothing -> SongDictionary $ M.insert title [song] songDict
  where
    title = songTitle song

songToDocument :: Song -> Document
songToDocument Song{..} = Document songTitle songBody
  where
    songBody = songChorus ++ (songVerses >>= verseLines)
