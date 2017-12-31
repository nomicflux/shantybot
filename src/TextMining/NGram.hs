{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TextMining.NGram where

import Control.Monad (msum)
import Data.List (tails, foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

data NGram = NGram { getNGram :: Text
                   , tokens :: Int
                   }
  deriving (Show, Eq, Ord)

instance Monoid NGram where
  mempty = NGram "" 0
  mappend (NGram text1 tokens1) (NGram text2 tokens2) =
    NGram (text1 <> comboText <> text2) (tokens1 + tokens2)

ngramToText :: NGram -> Text
ngramToText = getNGram

singleton :: Text -> NGram
singleton = (flip NGram) 1

comboText :: Text
comboText = " "

mkNGram :: Int -> [NGram] -> Maybe NGram
mkNGram n doc
  | length doc >= n = Just . foldl' (<>) mempty . take n $ doc
  | otherwise = Nothing

genNGrams :: [NGram] -> Int -> [NGram]
genNGrams doc n = mapMaybe (mkNGram n) (take n <$> tails doc)

genNMGrams :: Int -> Int -> [NGram] -> [NGram]
genNMGrams minGrams maxGrams doc = msum . map (genNGrams doc) $ [minGrams .. maxGrams]
