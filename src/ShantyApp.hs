{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ShantyApp where

import Prelude hiding (readFile, writeFile)
import Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Logging as L
import Control.Monad ((<=<))
import Control.Monad.Loops (iterateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Monoid ((<>))
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

import Song.Song (Song(..), songToText)
import TextMining.Document (ToDocument(..))
import TextMining.DocumentReader (DocumentReader)
import TextMining.Corpus (Corpus)
import TextMining.TfIdf (TfIdf, matchInCorpus)
import TextRetrieval.RetrievalService
import TextRetrieval.RetrievalReader (mkConfig, DocumentRetrieval(..))
import TextRetrieval.RetrievalEndpoint
import Twitter.Internal.AccessInfo
import Twitter.Streaming
import Twitter.Tweet

isMention :: Text -> Bool
isMention = (== "@") . T.take 1

filterOutMention :: Text -> Text
filterOutMention = T.unwords . filter (not . isMention) . T.words

wordsToLimit :: Int -> Text -> Text
wordsToLimit n text = go (filter (/= "") . T.words $ text) "" 0
  where
    go [] res _ = T.strip res
    go (x:xs) res m =
      if m + T.length x > n then res else go xs (res <> " " <> x) (m + T.length x + 1)

respondToTweet :: Monad m => Corpus Song -> TfIdf -> Tweet -> DocumentReader m (Maybe (Tweet, Text))
respondToTweet corpus tfidf tweet = do
  songs <- matchInCorpus corpus (filterOutMention $ tweetText tweet) tfidf
  return $ songMsg <$> listToMaybe songs
  where
    respondToPerson = "@" <> tweetUserScreenName (tweetUser tweet) <> " "
    name = T.unwords . take 1 . T.words . tweetUserName . tweetUser $ tweet
    greeting = "Ahoy " <> name <> ", perhaps you want: "
    songMsg s = let msg = greeting <> songTitle s <> "? "
                    size = T.length msg
                    songText = songToText s
                in (tweet, respondToPerson <> msg <> wordsToLimit (140 - size - 4) songText <> " ...")

runCycle :: (MonadIO m, MonadReader (DocumentRetrieval Song) m) =>
  Manager ->
  (Maybe Text -> m Request) ->
  (Tweet -> Text -> m Request) ->
  Maybe Text ->
  DocumentReader m (Maybe Text)
runCycle manager get post prevId = do
  docState <- lift ask
  corpus <- liftIO $ STM.readTVarIO (rcDocs docState)
  tfidf <- liftIO $ STM.readTVarIO (rcTfidf docState)
  getter <- lift $ get prevId
  (tweets :: [Tweet]) <- queryEndpoint manager getter
  liftIO $ L.log' . T.pack $ show tweets
  maybeResponses <- mapM (respondToTweet corpus tfidf) tweets
  let responses = catMaybes maybeResponses
  liftIO $ L.debug' . T.pack $ show responses
  lift $ mapM_ (useEndpoint manager <=< uncurry post) responses
  let
    ids :: [Integer]
    ids = map (read . T.unpack . tweetId) tweets
    maxId :: Maybe Integer
    maxId = if null ids then Nothing else Just $ maximum ids
  return $ T.pack . show <$> maxId

mainProg :: (MonadIO m, MonadReader (DocumentRetrieval Song) m) => Config -> DocumentReader m ()
mainProg cfg = do
  manager <- liftIO $ newManager tlsManagerSettings
  let
    idFile = "data/lastId"
    returnId prevId Nothing  = prevId
    returnId _ i@(Just _) = i
    fromId prevId = do
      nextId <- runCycle manager (getMentions cfg) (postReply cfg) prevId
      maybe (return ()) (liftIO . writeFile idFile) nextId
      liftIO $ threadDelay $ 1000 * 1000 * (configPollFreq cfg)
      return $ returnId prevId nextId
  fileExists <- liftIO $ doesFileExist idFile
  firstId <- liftIO $ if fileExists then Just <$> readFile idFile else return Nothing
  _ <- iterateM_ fromId firstId
  return ()

runService :: (MonadBaseControl IO m, MonadIO m) => DocumentReader m ()
runService = do
  settings <- ask
  cfg <- getConfig
  let
    songAPI = documentAPI :: S.Proxy (DocumentAPI Song)
    logger Nothing = L.withStdoutLogging
    logger (Just logfile) = L.withFileLogging logfile
  logger (configLogfile cfg) $ do
    (corpus, tfidf) <- getTfIdfFromDir songDirectory
    serverCfg <- mkConfig corpus tfidf
    let policy = Cors.simpleCorsResourcePolicy { Cors.corsRequestHeaders = [ "content-type" , "Accept", "Method" ]
                                               , Cors.corsMethods = Cors.simpleMethods
                                               }
    _ <- liftIO . forkIO $ runReaderT (runReaderT (mainProg cfg) settings) serverCfg
    liftIO $ Warp.run 8803 . RL.logStdoutDev . Cors.cors (const $ Just policy) . SO.provideOptions songAPI . S.serve documentAPI $ documentServer settings serverCfg
