{-# LANGUAGE OverloadedStrings #-}
module Twitter.Types where

import Data.ByteString (ByteString)

newtype URL = URL ByteString
  deriving (Show, Eq)
data Method = GET | POST | PUT | DELETE
  deriving (Show, Eq)

data Request = Request { requestMethod :: Method
                       , requestURL :: URL
                       , requestParams :: [(ByteString, ByteString)]
                       }
  deriving (Show, Eq)

data Secret = Secret { secretConsumer :: ByteString
                     , secretToken :: Maybe ByteString
                     }
  deriving (Show, Eq)

data Token = Token { tokenConsumer :: ByteString
                   , tokenAccess :: Maybe ByteString
                   }
  deriving (Show, Eq)
