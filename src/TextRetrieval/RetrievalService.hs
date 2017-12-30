{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TextRetrieval.RetrievalService where

import Prelude hiding (readFile, writeFile)

import System.Directory
import qualified Control.Logging as L
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.UUID (toString)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Yaml (encodeFile, decodeFileEither, FromJSON, ToJSON)
import System.Random (randomIO)

import TextMining.Document (ToDocument(..))
import TextMining.DocumentReader (DocumentReader, DocumentSettings(..))
import TextMining.Corpus (Corpus, genCorpus)
import TextMining.TfIdf (TfIdf, genTfIdf)

writeNewDoc :: (ToJSON a, MonadIO m, MonadReader DocumentSettings m) => a -> m ()
writeNewDoc text = do
  dir <- asks documentPath
  name <- toString <$> liftIO randomIO
  let fileName = dir <> "/" <> name <> ".txt"
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

getTfIdfFromDir :: (MonadIO m, FromJSON a, ToDocument a) => DocumentReader m (Corpus a, TfIdf)
getTfIdfFromDir = do
  dir <- asks documentPath
  docs <- liftIO $ getFiles dir
  corpus <- genCorpus docs
  let tfidf = genTfIdf corpus
  return (corpus, tfidf)
