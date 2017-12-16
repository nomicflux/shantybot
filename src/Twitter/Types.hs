{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Twitter.Types where

import Data.Aeson ((.:), (.:?), parseJSON, withObject, FromJSON)
import Data.ByteString (ByteString, pack)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

newtype URL = URL ByteString
  deriving (Show, Eq)
data Method = GET | POST | PUT | DELETE
  deriving (Show, Eq)

data Request = Request { requestMethod :: Method
                       , requestURL :: URL
                       , requestParams :: [(ByteString, ByteString)]
                       }
  deriving (Show, Eq)

data Owner = Owner { ownerName :: Text
                   , ownerId :: Int
                   }
  deriving (Show, Eq)

instance FromJSON Owner where
  parseJSON = withObject "OwnerObject" $ \o -> do
    ownerName <- o .: "owner"
    ownerId <- o .: "owner_id"
    return $ Owner{..}

data Secret = Secret { secretConsumer :: ByteString
                     , secretToken :: Maybe ByteString
                     }
  deriving (Show, Eq)

instance FromJSON Secret where
  parseJSON = withObject "SecretObject" $ \o -> do
    secretConsumer <- encodeUtf8 <$> o .: "consumer_secret"
    secretToken <- (fmap encodeUtf8) <$> o .:? "access_secret"
    return $ Secret{..}

data Token = Token { tokenConsumer :: ByteString
                   , tokenAccess :: Maybe ByteString
                   }
  deriving (Show, Eq)

instance FromJSON Token where
  parseJSON = withObject "TokenObject" $ \o -> do
    tokenConsumer <- encodeUtf8 <$> o .: "consumer_key"
    tokenAccess <- (fmap encodeUtf8) <$> o .:? "access_token"
    return $ Token{..}
