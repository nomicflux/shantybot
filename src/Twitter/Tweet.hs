{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Twitter.Tweet where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)

type TweetText = Text

data TweetUser = TweetUser { tweetUserId :: TweetText
                           , tweetUserName :: TweetText
                           , tweetUserScreenName :: TweetText
                           }
  deriving (Eq, Show)

data Tweet = Tweet { tweetId :: TweetText
                   , tweetText :: TweetText
                   , tweetUser :: TweetUser
                   }
  deriving (Eq, Show)

instance FromJSON TweetUser where
  parseJSON = withObject "TweetUserObject" $ \o -> do
    tweetUserId <- o .: "id_str"
    tweetUserName <- o .: "name"
    tweetUserScreenName <- o .: "screen_name"
    return TweetUser{..}

instance FromJSON Tweet where
  parseJSON = withObject "TweetObject" $ \o -> do
    tweetId <- o .: "id_str"
    tweetText <- o .: "text"
    tweetUser <- o .: "user"
    return Tweet{..}
