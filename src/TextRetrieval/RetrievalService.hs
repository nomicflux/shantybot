{-# LANGUAGE OverloadedStrings #-}

module TextRetrieval.RetrievalService where

import Prelude hiding (readFile, writeFile)

import System.Directory
import qualified Control.Logging as L
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID (toString)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (pack, Text, replace, unpack)
import Data.Text.IO (readFile, writeFile)
import Data.Yaml (encodeFile, decodeFileEither, FromJSON, ToJSON)
import System.Random (randomIO)

import Song.Song
import TextMining.Document (mkDocument, Document(..))
import TextMining.DocumentReader (DocumentReader)
import TextMining.Corpus (Corpus, genCorpus)
import TextMining.TfIdf (TfIdf, genTfIdf, matchPhrase)

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

getTfIdfFromDir :: MonadIO m => FilePath -> DocumentReader m (SongDictionary, Corpus, TfIdf)
getTfIdfFromDir dir = do
  songs <- liftIO $ getFiles dir
  let docs = songToDocument <$> songs
  corpus <- genCorpus docs
  let tfidf = genTfIdf corpus
  return (mkDictionary songs, corpus, tfidf)
