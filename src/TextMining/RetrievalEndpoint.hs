{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TextMining.RetrievalEndpoint where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Control.Concurrent.STM as STM
import qualified Control.Logging as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Servant
import Servant.Server (enter)
import Servant.API

import TextMining.Document
import TextMining.RetrievalReader
import TextMining.RetrievalService

type DocumentAPI = "gettitles" :> Get '[JSON] [Text]
              :<|> "getsongs" :> Get '[JSON] Documents
              :<|> "get" :> Capture "name" Text :> Get '[JSON] (Maybe Document)
              :<|> "add" :> Capture "name" Text :> ReqBody '[JSON] Phrase :> Post '[JSON] Bool
              :<|> "match" :> ReqBody '[JSON] Phrase :> Post '[JSON] (Maybe Document)

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

documentServer :: DocumentRetrieval -> Server DocumentAPI
documentServer cfg = enter (readerTToHandler cfg) documentServerT

documentServerT :: ServerT DocumentAPI AppM
documentServerT = getAllTitles
             :<|> getAllDocs
             :<|> getDoc
             :<|> addDoc
             :<|> matchToDocs

getAllTitles :: AppM [Text]
getAllTitles = do
  docVar <- rcDocs <$> ask
  M.keys <$> (liftIO . STM.readTVarIO $ docVar)

getAllDocs :: AppM Documents
getAllDocs = do
  docVar <- rcDocs <$> ask
  liftIO . STM.readTVarIO $ docVar

getDoc :: Text -> AppM (Maybe Document)
getDoc name = do
  docVar <- rcDocs <$> ask
  docMap <- liftIO . STM.readTVarIO $ docVar
  return $ M.lookup name docMap

addDoc :: Text -> Phrase -> AppM Bool
addDoc name (Phrase text) = do
  docVar <- ask
  L.debug' $ "Adding " <> name <> " with text " <> text
  updateDocs name text docVar
  return True

matchToDocs :: Phrase -> AppM (Maybe Document)
matchToDocs (Phrase phrase) = do
  liftIO $ L.debug' phrase
  tfidfVar <- rcTfidf <$> ask
  docsVar <- rcDocs <$> ask
  tfidf <- liftIO $ STM.readTVarIO tfidfVar
  docs <- liftIO $ STM.readTVarIO docsVar
  let name = matchPhrase phrase $ tfidf
      doc = name >>= (flip M.lookup) docs
  return doc
