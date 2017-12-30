{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TextRetrieval.RetrievalReader where

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, runReader, ask)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Data.Yaml (ToJSON)

import Servant (Handler, (:~>)(..))

import TextMining.Document (ToDocument(..))
import TextMining.DocumentReader (DocumentReader, DocumentSettings(..))
import TextMining.Corpus (Corpus, addToCorpus)
import TextMining.TfIdf (TfIdf, genTfIdf)
import TextRetrieval.RetrievalService (writeNewDoc)

type AppM a = DocumentReader (ReaderT (DocumentRetrieval a) IO)

getDocSettings :: ToDocument a => AppM a DocumentSettings
getDocSettings = ask

getDocVars :: ToDocument a => AppM a (DocumentRetrieval a)
getDocVars = lift ask

readerTToHandler :: ToDocument a => DocumentSettings -> DocumentRetrieval a -> AppM a :~> Handler
readerTToHandler docCfg cfg = NT (\r -> liftIO $ runReaderT (runReaderT r docCfg) cfg)

data DocumentRetrieval a = DocumentRetrieval { rcDocs :: TVar (Corpus a)
                                             , rcTfidf :: TVar TfIdf
                                             }

mkConfig :: (MonadIO m, ToDocument a) => Corpus a -> TfIdf -> m (DocumentRetrieval a)
mkConfig docs' tfidf' = do
  docs <- liftIO $ STM.newTVarIO docs'
  tfidf <- liftIO $ STM.newTVarIO tfidf'
  return $ DocumentRetrieval docs tfidf

updateDocs :: (MonadIO m, ToJSON a, ToDocument a) => a -> DocumentRetrieval a -> DocumentReader m ()
updateDocs thisDoc docState = do
  docSettings <- ask
  let docVar = rcDocs docState
      tfidfVar = rcTfidf docState
  liftIO $ STM.atomically $ do
    oldCorpus <- STM.readTVar docVar
    let newCorpus = runReader (addToCorpus oldCorpus thisDoc) docSettings
    STM.writeTVar docVar newCorpus
    let newTfidf = genTfIdf newCorpus
    STM.writeTVar tfidfVar $ newTfidf
  writeNewDoc thisDoc
