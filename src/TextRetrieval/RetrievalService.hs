{-# LANGUAGE OverloadedStrings #-}

module TextRetrieval.RetrievalService where

import Prelude hiding (readFile, writeFile)

import System.Directory
import qualified Control.Logging as L
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID (toString)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Yaml (encodeFile, decodeFileEither, FromJSON, ToJSON)
import System.Random (randomIO)

import TextMining.Document (ToDocument(..))
import TextMining.DocumentReader (DocumentReader)
import TextMining.Corpus (Corpus, genCorpus)
import TextMining.TfIdf (TfIdf, genTfIdf)

songDirectory :: FilePath
songDirectory = "data/songs"

writeNewDoc :: ToJSON a => MonadIO m => a -> m ()
writeNewDoc text = do
  name <- toString <$> liftIO randomIO
  let fileName = songDirectory <> "/" <> name <> ".txt"
  L.log' $ pack $ "Writing file " <> fileName
  liftIO $ encodeFile fileName text

fileToDoc :: (MonadIO m, FromJSON a) => FilePath -> FilePath -> m (Maybe a)
fileToDoc dir fileName =
  let filePath = dir <> "/" <> fileName
  in do
    song <- liftIO $ decodeFileEither filePath
    case song of
      Left err -> liftIO $ L.warn' (pack . show $ err) >> return Nothing
      Right text -> return $ Just text

getFiles :: (MonadIO m, FromJSON a) => FilePath -> m [a]
getFiles dir = do
  files <- liftIO $ listDirectory dir
  catMaybes <$> mapM (fileToDoc dir) files

getTfIdfFromDir :: (MonadIO m, FromJSON a, ToDocument a) => FilePath -> DocumentReader m (Corpus a, TfIdf)
getTfIdfFromDir dir = do
  docs <- liftIO $ getFiles dir
  corpus <- genCorpus docs
  let tfidf = genTfIdf corpus
  return (corpus, tfidf)
