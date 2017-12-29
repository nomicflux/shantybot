{-# LANGUAGE OverloadedStrings #-}

module TextMining.RetrievalService where

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
import Data.Yaml (encodeFile, decodeFileEither)
import System.Random (randomIO)

import TextMining.Document (mkDocument, Document(..))
import TextMining.DocumentReader (DocumentReader)
import TextMining.Corpus (Corpus, genCorpus)
import TextMining.TfIdf (TfIdf, genTfIdf, matchPhrase)

songDirectory :: FilePath
songDirectory = "data/songs"

writeNewDoc :: MonadIO m => Document -> m ()
writeNewDoc text = do
  name <- toString <$> (liftIO randomIO)
  let fileName = songDirectory <> "/" <> name <> ".txt"
  L.log' $ pack $ "Writing file " <> fileName
  liftIO $ encodeFile fileName text

fileToDoc :: MonadIO m => FilePath -> FilePath -> m (Maybe Document)
fileToDoc dir fileName =
  let docName = pack $ takeWhile (/= '.') fileName
      filePath = dir <> "/" <> fileName
  in do
    doc <- liftIO $ decodeFileEither filePath
    case doc of
      Left err -> liftIO $ L.warn' (pack $ show $ err) >> return Nothing
      Right text -> return $ Just text

getFiles :: MonadIO m => FilePath -> m [Document]
getFiles dir = do
  files <- liftIO $ listDirectory dir
  catMaybes <$> mapM (fileToDoc dir) files

getTfIdfFromDir :: MonadIO m => FilePath -> DocumentReader m (Corpus, TfIdf)
getTfIdfFromDir dir = do
  docs <- liftIO $ getFiles dir
  corpus <- genCorpus docs
  let docMap = M.fromList $ (\d -> (docName d, d)) <$> docs
      tfidf = genTfIdf corpus
  return (corpus, tfidf)
