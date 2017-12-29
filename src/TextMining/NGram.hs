{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TextMining.NGram where

import Control.Monad (msum)
import Data.List (tails, foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

newtype NGram = NGram { getNGram :: Text }
  deriving (Show, Eq, Ord)

instance Monoid NGram where
  mempty = NGram ""
  mappend (NGram text1) (NGram text2) = NGram (text1 <> comboText <> text2)

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
