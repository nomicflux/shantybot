{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Song.Song where

import Data.Aeson (FromJSON, parseJSON, (.:), withObject, ToJSON, toJSON, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import TextMining.Document (Document(..), Line(..), ToDocument(..))

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

chorusToText :: Song -> Text
chorusToText = T.intercalate "\n" . (lineText <$>) . songChorus

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

songToDocument :: Song -> Document
songToDocument Song{..} = Document songTitle songBody
  where
    songBody = songChorus ++ (songVerses >>= verseLines)

instance ToDocument Song where
  toDocument = songToDocument
