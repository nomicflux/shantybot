{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TextMining.RetrievalReader where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Data.Map (Map)
import Data.Text (Text)

import Servant (ServantErr, Handler, (:~>)(..))

import TextMining.Document (Document, TfIdf)

type AppM = ReaderT RetrievalConfig IO

readerTToHandler :: RetrievalConfig -> AppM :~> Handler
readerTToHandler cfg = NT (\r -> liftIO $ runReaderT r cfg)

data RetrievalConfig = RetrievalConfig { rcDocs :: TVar (Map Text Text)
                                       , rcTfidf :: TVar (Maybe TfIdf)
                                       }

mkConfig :: MonadIO m => Map Text Text -> TfIdf -> m RetrievalConfig
mkConfig docs tfidf = do
  documents <- liftIO $ STM.newTVarIO docs
  tfidf <- liftIO $ STM.newTVarIO (Just tfidf)
  return $ RetrievalConfig documents tfidf
