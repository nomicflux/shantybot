{-# LANGUAGE OverloadedStrings #-}
module TextMining.RetrievalService where

import Prelude hiding (readFile, writeFile)

import System.Directory
import qualified Control.Logging as L
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (pack, Text, replace, unpack)
import Data.Text.IO (readFile, writeFile)

import TextMining.Document

songDirectory :: FilePath
songDirectory = "data/songs"

writeNewDoc :: MonadIO m => Text -> Text -> m ()
writeNewDoc name text = do
  let fileName = songDirectory <> "/" <> (unpack $ replace " " "" name) <> ".txt"
  L.log' $ pack $ "Writing file " <> fileName
  liftIO $ writeFile fileName text

fileToDoc :: FilePath -> FilePath -> IO Document
fileToDoc dir fileName =
  let docName = pack $ takeWhile (/= '.') fileName
      filePath = dir <> "/" <> fileName
  in (mkDocument docName) <$> readFile filePath

getFiles :: FilePath -> IO [Document]
getFiles dir = do
  files <- listDirectory dir
  mapM (fileToDoc dir) files

getTfIdfFromDir :: FilePath -> Int -> Int -> IO (Documents, TfIdf)
getTfIdfFromDir dir minGrams maxGrams = do
  docs <- getFiles dir
  let docMap = M.fromList $ (\d -> (docName d, d)) <$> docs
      tfidf = genTfIdfFromDocs minGrams maxGrams docs
  return (docMap, tfidf)
