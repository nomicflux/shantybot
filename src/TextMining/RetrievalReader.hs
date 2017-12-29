{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TextMining.RetrievalReader where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, runReader, ask)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Servant (ServantErr, Handler, (:~>)(..))

import TextMining.Document (Document, mkDocument)
import TextMining.DocumentReader (DocumentReader, DocumentSettings)
import TextMining.Corpus (Corpus, addToCorpus)
import TextMining.TfIdf (TfIdf, genTfIdf)
import TextMining.RetrievalService (songDirectory, writeNewDoc)

type AppM = DocumentReader (ReaderT DocumentRetrieval IO)

getDocSettings :: AppM DocumentSettings
getDocSettings = ask

getDocVars :: AppM DocumentRetrieval
getDocVars = lift ask

readerTToHandler :: DocumentSettings -> DocumentRetrieval -> AppM :~> Handler
readerTToHandler docCfg cfg = NT (\r -> liftIO $ runReaderT (runReaderT r docCfg) cfg)

data DocumentRetrieval = DocumentRetrieval { rcDocs :: TVar Corpus
                                           , rcTfidf :: TVar TfIdf
                                           }

mkConfig :: MonadIO m => Corpus -> TfIdf -> m DocumentRetrieval
mkConfig docs tfidf = do
  documents <- liftIO $ STM.newTVarIO docs
  tfidf <- liftIO $ STM.newTVarIO tfidf
  return $ DocumentRetrieval documents tfidf

updateDocs :: MonadIO m => Document -> DocumentRetrieval -> DocumentReader m ()
updateDocs thisDoc docState = do
  docSettings <- ask
  let docVar = rcDocs docState
      tfidfVar = rcTfidf docState
  synchronized <- liftIO $ STM.atomically $ do
    oldCorpus <- STM.readTVar $ docVar
    oldTfidf <- STM.readTVar $ tfidfVar
    let
      newCorpus :: Corpus
      newCorpus = runReader (addToCorpus oldCorpus thisDoc) docSettings
    STM.writeTVar docVar newCorpus
    let newTfidf = genTfIdf newCorpus
    STM.writeTVar tfidfVar $ newTfidf
  liftIO $ writeNewDoc thisDoc
