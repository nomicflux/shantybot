{-# LANGUAGE OverloadedStrings #-}
module TextMining.RetrievalService where

import Prelude hiding (readFile)

import System.Directory
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (pack, Text)
import Data.Text.IO (readFile)

import TextMining.Document

songDirectory :: FilePath
songDirectory = "data/songs"

fileToDoc :: FilePath -> FilePath -> IO (Text, Document)
fileToDoc dir fileName =
  let docName = pack $ takeWhile (/= '.') fileName
      filePath = dir <> "/" <> fileName
  in (\t -> (t, mkDocument docName t)) <$> readFile filePath

getFiles :: FilePath -> IO [(Text, Document)]
getFiles dir = do
  files <- listDirectory dir
  mapM (fileToDoc dir) files

getTfIdfFromDir :: FilePath -> Int -> Int -> IO (M.Map Text Text, TfIdf)
getTfIdfFromDir dir minGrams maxGrams = do
  docs <- getFiles dir
  let docMap = M.fromList $ (\(t, d) -> (docName d, t)) <$> docs
      tfidf = genTfIdfFromDocs minGrams maxGrams (snd <$> docs)
  return (docMap, tfidf)
