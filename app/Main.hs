module Main where

import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Set as S

import TextMining.DocumentReader (DocumentSettings(..))
import ShantyApp

main :: IO ()
main = runReaderT runService (DocumentSettings 1 4 S.empty)
