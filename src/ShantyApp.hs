{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ShantyApp where

import Prelude hiding (readFile, writeFile)
import Control.Concurrent (threadDelay)
import qualified Control.Logging as L
import Control.Monad ((<=<))
import Control.Monad.Loops (iterateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text as T
import Pipes.HTTP
import System.Directory (doesFileExist)

import TextMining.Document
import TextMining.RetrievalService
import Twitter.Internal.AccessInfo
import Twitter.Streaming
import Twitter.Tweet

isMention :: Text -> Bool
isMention = (== "@") . T.take 1

filterOutMention :: Text -> Text
filterOutMention = T.unwords . filter (not . isMention) . T.words

capitalChar :: Char -> Bool
capitalChar c = (c >= 'A') && (c <= 'Z')

unPascalCase :: Text -> Text
unPascalCase text = go (breaker text) ""
  where
    breaker = T.break capitalChar
    go (n, "") res = T.strip $ res <> n
    go (n, m) res = go (breaker $ T.drop 1 m) (res <> n <> " " <> T.take 1 m)

respondToTweet :: TfIdf -> Tweet -> Maybe (Tweet, Text)
respondToTweet tfidf tweet =
  (\s -> (tweet, respondToPerson <> greeting <> s <> "?")) <$> song
  where
    respondToPerson :: Text
    respondToPerson = "@" <> (tweetUserScreenName $ tweetUser tweet)  <> " "
    greeting :: Text
    greeting = "Ahoy " <> (tweetUserName $ tweetUser tweet) <> ", perhaps you want: "
    song :: Maybe Text
    song = unPascalCase <$> matchPhrase (filterOutMention $ tweetText tweet) tfidf

runCycle :: MonadIO m => Manager -> TfIdf ->
  (Maybe Text -> m Request) ->
  (Tweet -> Text -> m Request) ->
  Maybe Text -> m (Maybe Text)
runCycle manager tfidf get post prevId = do
  getter <- get prevId
  (tweets :: [Tweet]) <- queryEndpoint manager getter
  liftIO $ L.log' . T.pack $ show tweets
  let responses = mapMaybe (respondToTweet tfidf) tweets
  liftIO $ L.debug' . T.pack $ show responses
  mapM_ (useEndpoint manager <=< uncurry post) responses
  let
    ids :: [Integer]
    ids = map (read . T.unpack . tweetId) tweets
    maxId :: Maybe Integer
    maxId = if null ids then Nothing else Just $ maximum ids
  return $ T.pack . show <$> maxId

runService :: (MonadBaseControl IO m, MonadIO m) => m ()
runService = do
  cfg <- getConfig
  let
    logger Nothing = L.withStdoutLogging
    logger (Just logfile) = L.withFileLogging logfile
  let mainProg = do
        manager <- newManager tlsManagerSettings
        tfidf <- getTfIdfFromDir songDirectory 1 4
        let
          idFile = "data/lastId"
          returnId prevId Nothing  = prevId
          returnId _ i@(Just _) = i
          fromId prevId = do
            nextId <- runCycle manager tfidf (getMentions cfg) (postReply cfg) prevId
            maybe (return ()) (writeFile idFile) nextId
            threadDelay (1000 * 1000 * (configPollFreq cfg))
            return $ returnId prevId nextId
        fileExists <- doesFileExist idFile
        firstId <- if fileExists then Just <$> readFile idFile else return Nothing
        iterateM_ fromId firstId
        return ()
  logger (configLogfile cfg) $ liftIO mainProg
