{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TextMining.RetrievalEndpoint where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Control.Concurrent.STM as STM
import qualified Control.Logging as L
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Servant
import Servant.Server (enter)
import Servant.API

import TextMining.Document (Document, Line(..), mkDocument)
import TextMining.DocumentReader (DocumentSettings)
import TextMining.Corpus (documentsFromCorpus, Corpus(..))
import TextMining.TfIdf (matchPhrase)
import TextMining.RetrievalReader
import TextMining.RetrievalService

type DocumentAPI = "gettitles" :> Get '[JSON] [Text]
              :<|> "get" :> Capture "name" Text :> Get '[JSON] [Document]
              :<|> "add" :> Capture "name" Text :> ReqBody '[JSON] TextWrapper :> Post '[JSON] Bool
              :<|> "match" :> ReqBody '[JSON] Line :> Post '[JSON] [Document]

newtype TextWrapper = TextWrapper Text

instance FromJSON TextWrapper where
  parseJSON = withObject "TextWrapperObject" $ \obj ->
    TextWrapper <$> obj .: "value"

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

documentServer :: DocumentSettings -> DocumentRetrieval -> Server DocumentAPI
documentServer docCfg cfg = enter (readerTToHandler docCfg cfg) documentServerT

documentServerT :: ServerT DocumentAPI AppM
documentServerT = getAllTitles
             :<|> getDoc
             :<|> addDoc
             :<|> matchToDocs

getAllTitles :: AppM [Text]
getAllTitles = do
  docVar <- rcDocs <$> getDocVars
  M.keys . corpusItems <$> (liftIO . STM.readTVarIO $ docVar)

getDoc :: Text -> AppM [Document]
getDoc name = do
  docVar <- rcDocs <$> getDocVars
  corpus <- liftIO . STM.readTVarIO $ docVar
  --L.debug' $ "Corpus" <> T.pack (show corpus)
  return $ documentsFromCorpus corpus name

addDoc :: Text -> TextWrapper -> AppM Bool
addDoc name (TextWrapper text) = do
  let doc = mkDocument name text
  docVar <- getDocVars
  L.debug' $ "Adding " <> name <> " with text " <> text
  updateDocs doc docVar
  return True

matchToDocs :: Line -> AppM [Document]
matchToDocs (Line phrase) = do
  liftIO $ L.debug' phrase
  tfidfVar <- rcTfidf <$> getDocVars
  docsVar <- rcDocs <$> getDocVars
  tfidf <- liftIO $ STM.readTVarIO tfidfVar
  corpus <- liftIO $ STM.readTVarIO docsVar
  name <- matchPhrase phrase $ tfidf
  let docs = maybe [] (documentsFromCorpus corpus) name
  return docs
