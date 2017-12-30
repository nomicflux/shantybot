{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Logging as L
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y

import TextMining.DocumentReader (DocumentSettings(..))
import ShantyApp

main :: IO ()
main = L.withStdoutLogging $ do
  attemptSettings <- Y.decodeFileEither "config/settings.yaml"
  settings <- case attemptSettings of
    Left err -> do
      let msg = Y.prettyPrintParseException err
      L.warn' . T.pack $  msg
      fail msg
    Right res -> return res
  stopwordText <- TIO.readFile "data/stopwords.txt"
  let stopWords = S.fromList $ T.splitOn "," stopwordText
      fullSettings = settings { stopWords = stopWords }
  L.log' . T.pack . show $ fullSettings
  runReaderT runService fullSettings
