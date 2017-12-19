{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Twitter.Internal.AccessInfo where

import Data.Yaml ((.:?), (.!=), FromJSON, parseJSON, withObject)
import qualified Data.Yaml as Yaml
import Data.ByteString (ByteString)
import Data.Text (Text)

import Twitter.Types

data Config = Config { configToken :: Token
                     , configSecret :: Secret
                     , configOwner :: Owner
                     , configLogfile :: Maybe String
                     , configPollFreq :: Int
                     }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "ConfigObject" $ \o -> do
    configToken <- parseJSON (Yaml.Object o)
    configSecret <- parseJSON (Yaml.Object o)
    configOwner <- parseJSON (Yaml.Object o)
    configLogfile <- o .:? "logfile"
    configPollFreq <- o .:? "pollfreq" .!= 60
    return $ Config{..}
