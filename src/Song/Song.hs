{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Song.Song where

import Control.Monad (join)
import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), withObject, ToJSON, toJSON, object, (.=))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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
                 , songChorus :: Maybe [Line]
                 , songVerses :: [Verse]
                 }

songToText :: Song -> Text
songToText Song{..} = T.intercalate "\n" $ lineText <$> songBody
  where
    songBody :: [Line]
    songBody = join $ (fromMaybe [] songChorus) : (verseLines <$> songVerses)

instance FromJSON Song where
  parseJSON = withObject "SongObject" $ \o -> do
    songTitle <- o .: "title"
    songChorus <- o .:? "chorus"
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
    --songBody = (fromMaybe [] songChorus) ++ (songVerses >>= verseLines)
    chorus = fromMaybe [] songChorus
    verses = verseLines <$> songVerses
    songBody = intercalate chorus verses

instance ToDocument Song where
  toDocument = songToDocument
