module Main where

import qualified Control.Logging as L
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Set as S

import TextMining.DocumentReader (DocumentSettings(..))
import ShantyApp

main :: IO ()
main =
  L.withStdoutLogging $ runReaderT runService (DocumentSettings 1 4 S.empty)
