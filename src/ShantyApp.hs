{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ShantyApp where

import Prelude hiding (readFile, writeFile)
import Control.Concurrent (threadDelay, forkIO)
import qualified Control.Logging as L
import Control.Monad ((<=<))
import Control.Monad.Loops (iterateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text as T
import Pipes.HTTP
import System.Directory (doesFileExist)
import qualified Servant as S
import qualified Network.Wai.Handler.Warp as Warp

import TextMining.Document
import TextMining.RetrievalService
import TextMining.RetrievalReader (mkConfig)
import TextMining.RetrievalEndpoint
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

wordsToLimit :: Int -> Text -> Text
wordsToLimit n text = go (filter (/= "") . T.words $ text) "" 0
  where
    go [] res _ = T.strip res
    go (x:xs) res m =
      if m + T.length x > n then res else go xs (res <> " " <> x) (m + T.length x + 1)

respondToTweet :: Map Text Text -> TfIdf -> Tweet -> Maybe (Tweet, Text)
respondToTweet docMap tfidf tweet = songMsg <$> song
  where
    respondToPerson = "@" <> (tweetUserScreenName $ tweetUser tweet)  <> " "
    name = T.unwords . take 1 . T.words . tweetUserName . tweetUser $ tweet
    greeting = "Ahoy " <> name <> ", perhaps you want: "
    song = matchPhrase (filterOutMention $ tweetText tweet) tfidf
    songMsg s = let msg = greeting <> unPascalCase s <> "? "
                    size = T.length msg
                    songText = fromMaybe "" $ M.lookup s docMap
                in (tweet, respondToPerson <> msg <> wordsToLimit (140 - size - 4) songText <> " ...")

runCycle :: MonadIO m => Manager -> Map Text Text -> TfIdf ->
  (Maybe Text -> m Request) ->
  (Tweet -> Text -> m Request) ->
  Maybe Text -> m (Maybe Text)
runCycle manager docMap tfidf get post prevId = do
  getter <- get prevId
  (tweets :: [Tweet]) <- queryEndpoint manager getter
  liftIO $ L.log' . T.pack $ show tweets
  let responses = mapMaybe (respondToTweet docMap tfidf) tweets
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
  (docs, tfidf) <- liftIO $ getTfIdfFromDir songDirectory 1 4
  serverCfg <- mkConfig docs tfidf
  liftIO . forkIO . Warp.run 8803 . S.serve documentAPI . documentServer $ serverCfg
  let
    logger Nothing = L.withStdoutLogging
    logger (Just logfile) = L.withFileLogging logfile
  let mainProg = do
        manager <- newManager tlsManagerSettings
        let
          idFile = "data/lastId"
          returnId prevId Nothing  = prevId
          returnId _ i@(Just _) = i
          fromId prevId = do
            nextId <- runCycle manager docs tfidf (getMentions cfg) (postReply cfg) prevId
            maybe (return ()) (writeFile idFile) nextId
            threadDelay (1000 * 1000 * (configPollFreq cfg))
            return $ returnId prevId nextId
        fileExists <- doesFileExist idFile
        firstId <- if fileExists then Just <$> readFile idFile else return Nothing
        iterateM_ fromId firstId
        return ()
  logger (configLogfile cfg) $ liftIO mainProg
