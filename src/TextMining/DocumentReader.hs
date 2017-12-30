{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TextMining.DocumentReader where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Set (Set)
import Data.Text (Text)

data DocumentSettings = DocumentSettings { minGrams :: Int
                                         , minStopwordGrams :: Int
                                         , maxGrams :: Int
                                         , stopWords :: Set Text
                                         }

type DocumentReader = ReaderT DocumentSettings
