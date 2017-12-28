{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TextMining.Corpus where

import Control.Monad (foldM)
import Control.Monad.Trans.Reader (ask)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>), mempty, mappend)
import Data.Text (Text)

import TextMining.Document
import TextMining.NGrams
import TextMining.Normalization
import TextMining.DocumentReader

newtype FreqMap = FreqMap (Map NGram Int)
  deriving (Show, Eq)

data CorpusItem = CorpusItem { corpusItemName :: Text
                             , corpusItemText :: [Line]
                             , corpusItemNGrams :: [NGram]
                             , corpusItemFreqs :: FreqMap
                             }
  deriving (Show, Eq)

newtype Corpus = Corpus {  corpusItems :: Map Text [CorpusItem] }
  deriving (Show, Eq)

instance Monoid Corpus where
  mempty = Corpus M.empty
  mappend (Corpus m1) (Corpus m2) = Corpus $ M.unionWith (++) m1 m2

corpusFromItem :: CorpusItem -> Corpus
corpusFromItem c@CorpusItem{..} =
  Corpus $ Map.fromList [(corpusItemName, c)]

genFreqMap :: [NGram] -> FreqMap
genFreqMap doc =
  FreqMap $ foldl' (\acc nGram -> M.insertWith (+) nGram 1 acc) M.empty nmGrams

mkCorpusItem :: (Monad m) => Document -> DocumentReader m CorpusItem
mkCorpusItem Document{..} = ask >>= $ \settings ->
  let newText = genNMGrams minGrams maxGrams . cleanText $ docText
      freqs = genFreqMap newText
  return $ CorpusItem docName docText newText freqs

addToCorpus :: (Monad m) => Corpus -> Document -> DocumentReader m Corpus
addToCorpus corpus doc = mkCorpusItem >>= $ \item ->
  return $ corpus <> (corpusFromItem item)

genCorpus :: (Monad m) => [Document] -> DocumentReader m Corpus
genCorpus = foldM addToCorpus mempty
