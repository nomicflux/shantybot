{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ShantyApp where

import Prelude hiding (readFile, writeFile)
import Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Logging as L
import Control.Monad ((<=<), forever)
import Control.Monad.Loops (iterateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text as T
import Pipes.HTTP
import System.Directory (doesFileExist)
import qualified Servant as S
import qualified Network.Wai.Middleware.Servant.Options as SO
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors

import TextMining.Document
import TextMining.DocumentReader (DocumentReader, DocumentSettings(..))
import TextMining.Corpus (Corpus(..))
import TextMining.TfIdf (TfIdf, matchPhrase)
import TextMining.RetrievalService
import TextMining.RetrievalReader (mkConfig, DocumentRetrieval(..))
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

respondToTweet :: Corpus -> TfIdf -> Tweet -> Maybe (Tweet, Text)
respondToTweet corpus tfidf tweet = songMsg <$> song
  where
    respondToPerson = "@" <> (tweetUserScreenName $ tweetUser tweet)  <> " "
    name = T.unwords . take 1 . T.words . tweetUserName . tweetUser $ tweet
    greeting = "Ahoy " <> name <> ", perhaps you want: "
    song = matchPhrase (filterOutMention $ tweetText tweet) tfidf
    songMsg s = let msg = greeting <> unPascalCase s <> "? "
                    size = T.length msg
                    songText = fromMaybe "" $ docText <$> M.lookup s docMap
                in (tweet, respondToPerson <> msg <> wordsToLimit (140 - size - 4) songText <> " ...")

runCycle :: (MonadIO m, MonadReader DocumentRetrieval m) =>
  Manager ->
  (Maybe Text -> m Request) ->
  (Tweet -> Text -> m Request) ->
  Maybe Text ->
  DocumentReader m (Maybe Text)
runCycle manager get post prevId = do
  docState <- lift $ ask
  corpus <- liftIO $ STM.readTVarIO (rcDocs docState)
  tfidf <- liftIO $ STM.readTVarIO (rcTfidf docState)
  getter <- lift $ get prevId
  (tweets :: [Tweet]) <- queryEndpoint manager getter
  liftIO $ L.log' . T.pack $ show tweets
  let responses = mapMaybe (respondToTweet corpus tfidf) tweets
  liftIO $ L.debug' . T.pack $ show responses
  lift $ mapM_ (useEndpoint manager <=< uncurry post) responses
  let
    ids :: [Integer]
    ids = map (read . T.unpack . tweetId) tweets
    maxId :: Maybe Integer
    maxId = if null ids then Nothing else Just $ maximum ids
  return $ T.pack . show <$> maxId

mainProg :: (MonadIO m, MonadReader DocumentRetrieval m) => Config -> DocumentReader m ()
mainProg cfg = do
  manager <- liftIO $ newManager tlsManagerSettings
  let
    idFile = "data/lastId"
    returnId prevId Nothing  = prevId
    returnId _ i@(Just _) = i
    fromId prevId = do
      nextId <- runCycle manager (getMentions cfg) (postReply cfg) prevId
      maybe (return ()) (liftIO . writeFile idFile) nextId
      liftIO $ threadDelay (1000 * 1000 * (configPollFreq cfg))
      return $ returnId prevId nextId
  fileExists <- liftIO $ doesFileExist idFile
  firstId <- liftIO $ if fileExists then Just <$> readFile idFile else return Nothing
  iterateM_ fromId firstId
  return ()

runService :: (MonadBaseControl IO m, MonadIO m) => DocumentReader m ()
runService = do
  settings <- ask
  cfg <- getConfig
  (docs, tfidf) <- getTfIdfFromDir songDirectory
  serverCfg <- mkConfig docs tfidf
  let
    logger Nothing = L.withStdoutLogging
    logger (Just logfile) = L.withFileLogging logfile
  logger (configLogfile cfg) $ do
    let policy = Cors.simpleCorsResourcePolicy { Cors.corsRequestHeaders = [ "content-type" , "Accept", "Method" ]
                                               , Cors.corsMethods = Cors.simpleMethods
                                               }
    liftIO . forkIO . runReaderT (mainProg cfg) $ serverCfg
    liftIO $ Warp.run 8803 . RL.logStdoutDev . Cors.cors (const $ Just policy) . SO.provideOptions documentAPI . S.serve documentAPI $ documentServer settings serverCfg
