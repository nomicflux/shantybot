{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module TextMining.Document where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), ToJSON, toJSON, object, (.=))
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

newtype Line = Line { lineText :: Text }
  deriving (Show, Eq)

instance FromJSON Line where
  parseJSON = withObject "LineObject" $ \o ->
    Line <$> o .: "line"

instance ToJSON Line where
  toJSON (Line text) = object [ "line" .= text ]

data Document = Document { docName :: Text
                         , docText :: [Line]
                         }
  deriving (Show, Eq)

instance FromJSON Document where
  parseJSON = withObject "DocObject" $ \o -> do
    docName <- o .: "name"
    docText <- o .: "text"
    return Document{..}

instance ToJSON Document where
  toJSON Document{..} = object [ "name" .= docName
                               , "text" .= docText
                               ]

type Documents = Map Text Document

mkDocument :: Text -> Text -> Document
mkDocument name text = Document name (Line <$> T.splitOn "\n" text)

class ToDocument a where
  toDocument :: a -> Document
  namedDocument :: Text -> a -> Document
  namedDocument title a = (toDocument a) { docName = title }
  toTextBody :: a -> Text
  toTextBody = T.intercalate "\n" . (lineText <$>) . docText . toDocument

instance ToDocument Document where
  toDocument = id

instance ToDocument Text where
  toDocument = mkDocument ""
