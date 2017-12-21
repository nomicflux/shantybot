{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TextMining.RetrievalReader where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Servant (ServantErr, Handler, (:~>)(..))

import TextMining.Document (Document, Documents, TfIdf(..), mkDocument, genTfIdfFromDocs)
import TextMining.RetrievalService (songDirectory, writeNewDoc)

type AppM = ReaderT DocumentRetrieval IO

readerTToHandler :: DocumentRetrieval -> AppM :~> Handler
readerTToHandler cfg = NT (\r -> liftIO $ runReaderT r cfg)

data DocumentRetrieval = DocumentRetrieval { rcDocs :: TVar Documents
                                           , rcTfidf :: TVar TfIdf
                                           }

mkConfig :: MonadIO m => Documents -> TfIdf -> m DocumentRetrieval
mkConfig docs tfidf = do
  documents <- liftIO $ STM.newTVarIO docs
  tfidf <- liftIO $ STM.newTVarIO tfidf
  return $ DocumentRetrieval documents tfidf

updateDocs :: (MonadIO m) => Text -> Text -> DocumentRetrieval -> m ()
updateDocs name text docState = do
  let docVar = rcDocs docState
      tfidfVar = rcTfidf docState
  synchronized <- liftIO $ STM.atomically $ do
    oldDocs <- STM.readTVar $ docVar
    oldTfidf <- STM.readTVar $ tfidfVar
    let
      thisDoc = mkDocument name text
      newDocs = thisDoc : (snd <$> M.toList oldDocs)
    STM.modifyTVar docVar $ (M.insert name thisDoc)
    let
      minGrams = tfidfMinGrams oldTfidf
      maxGrams = tfidfMaxGrams oldTfidf
      newTfidf = genTfIdfFromDocs minGrams maxGrams newDocs
    STM.writeTVar tfidfVar $ newTfidf
  liftIO $ writeNewDoc name text
