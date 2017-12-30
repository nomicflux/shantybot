{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module TextMining.DocumentReader where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), withObject, (.!=))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

data DocumentSettings = DocumentSettings { minGrams :: Int
                                         , minStopwordGrams :: Int
                                         , maxGrams :: Int
                                         , maxStopwordGrams :: Int
                                         , includeStopwords :: Bool
                                         , documentPath :: String
                                         , stopWords :: Set Text
                                         }
  deriving (Show)

instance FromJSON DocumentSettings where
  parseJSON = withObject "DocumentSettingsObject" $ \o -> do
    minGrams <- o .:? "minGrams" .!= 1
    minStopwordGrams <- o .:? "minStopwordGrams" .!= 1
    maxGrams <- o .: "maxGrams"
    maxStopwordGrams <- o .:? "maxStopwordGrams" .!= maxGrams
    includeStopwords <- o .:? "includeStopwords" .!= True
    documentPath <- o .: "documentPath"
    let stopWords = S.empty
    return $ DocumentSettings{..}

type DocumentReader = ReaderT DocumentSettings
