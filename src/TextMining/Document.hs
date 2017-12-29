{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module TextMining.Document where

import Control.Monad (msum)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), ToJSON, toJSON, object, (.=))
import Data.List (tails, length, foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))
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
