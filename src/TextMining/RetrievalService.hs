{-# LANGUAGE OverloadedStrings #-}
module TextMining.RetrievalService where

import Prelude hiding (readFile)

import System.Directory
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.IO (readFile)

import TextMining.Document

songDirectory :: FilePath
songDirectory = "data/songs"

fileToDoc :: FilePath -> FilePath -> IO Document
fileToDoc dir fileName =
  let docName = pack $ takeWhile (/= '.') fileName
      filePath = dir <> "/" <> fileName
  in (mkDocument docName) <$> readFile filePath

getFiles :: FilePath -> IO [Document]
getFiles dir = do
  files <- listDirectory dir
  mapM (fileToDoc dir) files

getTfIdfFromDir :: FilePath -> Int -> Int -> IO TfIdf
getTfIdfFromDir dir minGrams maxGrams =
  genTfIdfFromDocs minGrams maxGrams <$> getFiles dir
