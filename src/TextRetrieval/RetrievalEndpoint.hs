{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TextRetrieval.RetrievalEndpoint where

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

import Song.Song
import TextMining.Document (Document, Line(..), mkDocument)
import TextMining.DocumentReader (DocumentSettings)
import TextMining.Corpus (documentsFromCorpus, Corpus(..))
import TextMining.TfIdf (matchPhrase)
import TextRetrieval.RetrievalReader
import TextRetrieval.RetrievalService

type DocumentAPI = "gettitles" :> Get '[JSON] [Text]
              :<|> "get" :> Capture "name" Text :> Get '[JSON] [Document]
              :<|> "add" :> ReqBody '[JSON] Song :> Post '[JSON] Bool
              :<|> "match" :> ReqBody '[JSON] TextWrapper :> Post '[JSON] [Document]

newtype TextWrapper = TextWrapper Text

instance FromJSON TextWrapper where
  parseJSON = withObject "TextWrapperObject" $ \obj ->
    TextWrapper <$> obj .: "text"

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

documentServer :: DocumentSettings -> DocumentRetrieval -> Server DocumentAPI
documentServer docCfg cfg = enter (readerTToHandler docCfg cfg) documentServerT

documentServerT :: ServerT DocumentAPI AppM
documentServerT = getAllTitles
             :<|> getDoc
             :<|> addSong
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

addSong :: Song -> AppM Bool
addSong song = do
  docVar <- getDocVars
  updateDocs song docVar
  return True

matchToDocs :: TextWrapper -> AppM [Document]
matchToDocs (TextWrapper phrase) = do
  liftIO $ L.debug' phrase
  tfidfVar <- rcTfidf <$> getDocVars
  docsVar <- rcDocs <$> getDocVars
  tfidf <- liftIO $ STM.readTVarIO tfidfVar
  corpus <- liftIO $ STM.readTVarIO docsVar
  name <- matchPhrase phrase $ tfidf
  let docs = maybe [] (documentsFromCorpus corpus) name
  return docs
