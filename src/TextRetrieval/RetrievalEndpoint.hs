{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TextRetrieval.RetrievalEndpoint where

import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Logging as L
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), ToJSON)
import Data.Text (Text)

import Servant
import Servant.Server (enter)

import TextMining.Document (ToDocument(..))
import TextMining.DocumentReader (DocumentSettings)
import TextMining.Corpus (structuredDocsFromCorpus, getTitles)
import TextMining.TfIdf (matchInCorpus)
import TextRetrieval.RetrievalReader

type DocumentAPI a = "gettitles" :> Get '[JSON] [Text]
                     :<|> "get" :> Capture "name" Text :> Get '[JSON] [a]
                     :<|> "add" :> ReqBody '[JSON] a :> Post '[JSON] Bool
                     :<|> "match" :> ReqBody '[JSON] TextWrapper :> Post '[JSON] [a]

newtype TextWrapper = TextWrapper Text

instance FromJSON TextWrapper where
  parseJSON = withObject "TextWrapperObject" $ \obj ->
    TextWrapper <$> obj .: "text"

documentAPI :: (ToDocument a, FromJSON a, ToJSON a) => Proxy (DocumentAPI a)
documentAPI = Proxy

documentServer :: (ToDocument a, FromJSON a, ToJSON a) => DocumentSettings -> DocumentRetrieval a -> Server (DocumentAPI a)
documentServer docCfg cfg = enter (readerTToHandler docCfg cfg) documentServerT

documentServerT :: (ToDocument a, FromJSON a, ToJSON a) => ServerT (DocumentAPI a) (AppM a)
documentServerT = getAllTitles
             :<|> getDoc
             :<|> addSong
             :<|> matchToDocs

getAllTitles :: (ToDocument a, FromJSON a, ToJSON a) => AppM a [Text]
getAllTitles = do
  docVar <- rcDocs <$> getDocVars
  getTitles <$> (liftIO . STM.readTVarIO $ docVar)

getDoc :: (ToDocument a, FromJSON a, ToJSON a) => Text -> AppM a [a]
getDoc name = do
  docVar <- rcDocs <$> getDocVars
  corpus <- liftIO . STM.readTVarIO $ docVar
  --L.debug' $ "Corpus" <> T.pack (show corpus)
  return $ structuredDocsFromCorpus corpus name

addSong :: (ToDocument a, FromJSON a, ToJSON a) => a -> AppM a Bool
addSong song = do
  docVar <- getDocVars
  updateDocs song docVar
  return True

matchToDocs :: (ToDocument a, FromJSON a, ToJSON a) => TextWrapper -> AppM a [a]
matchToDocs (TextWrapper phrase) = do
  liftIO $ L.debug' phrase
  tfidfVar <- rcTfidf <$> getDocVars
  docsVar <- rcDocs <$> getDocVars
  tfidf <- liftIO $ STM.readTVarIO tfidfVar
  corpus <- liftIO $ STM.readTVarIO docsVar
  matchInCorpus corpus phrase tfidf
