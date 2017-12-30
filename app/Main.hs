{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Logging as L
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import TextMining.DocumentReader (DocumentSettings(..))
import ShantyApp

main :: IO ()
main = L.withStdoutLogging $ do
  stopwordText <- TIO.readFile "data/stopwords.txt"
  let stopwords = S.fromList $ T.splitOn "," stopwordText
  runReaderT runService (DocumentSettings 1 4 stopwords)
