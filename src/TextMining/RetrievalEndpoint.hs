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
              :<|> "getsongs" :> Get '[JSON] (Map Text Text)
              :<|> "get" :> Capture "name" Text :> Get '[JSON] (Maybe Text)
              :<|> "add" :> Capture "name" Text :> ReqBody '[PlainText] Text :> Post '[JSON] Bool
              :<|> "match" :> Capture "phrase" Text :> Get '[JSON] (Maybe Text)


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

getAllDocs :: AppM (Map Text Text)
getAllDocs = do
  docVar <- rcDocs <$> ask
  liftIO . STM.readTVarIO $ docVar

getDoc :: Text -> AppM (Maybe Text)
getDoc name = do
  docVar <- rcDocs <$> ask
  docMap <- liftIO . STM.readTVarIO $ docVar
  return $ M.lookup name docMap

addDoc :: Text -> Text -> AppM Bool
addDoc name text = do
  docVar <- ask
  L.debug' $ "Adding " <> name <> " with text " <> text
  updateDocs name text docVar
  return True

matchToDocs :: Text -> AppM (Maybe Text)
matchToDocs phrase = do
  tfidfVar <- rcTfidf <$> ask
  tfidf <- liftIO $ STM.readTVarIO tfidfVar
  return $ matchPhrase phrase tfidf
