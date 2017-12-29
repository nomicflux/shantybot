{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module TextMining.Corpus where

import Control.Monad (foldM, mapM, join)
import Control.Monad.Trans.Reader (ask)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Monoid ((<>), mempty, mappend)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import TextMining.Document
import TextMining.NGram
import TextMining.Normalization
import TextMining.DocumentReader

newtype FreqMap = FreqMap (Map NGram Int)
  deriving (Show, Eq)

instance Monoid FreqMap where
  mempty = FreqMap M.empty
  mappend (FreqMap m1) (FreqMap m2) = FreqMap $ M.unionWith (+) m1 m2

data CorpusItem = CorpusItem { corpusItemName :: Text
                             , corpusItemText :: [Line]
                             , corpusItemNGrams :: [NGram]
                             , corpusItemFreqs :: FreqMap
                             }
  deriving (Show, Eq)

newtype Corpus = Corpus { corpusItems :: Map Text [CorpusItem] }
  deriving (Show, Eq)

instance Monoid Corpus where
  mempty = Corpus M.empty
  mappend (Corpus m1) (Corpus m2) = Corpus $ M.unionWith (++) m1 m2

--getFreqsForName :: Corpus -> Text -> FreqMap
--getFreqsForName corpus =
  --maybe mempty (foldl' (<>) mempty) . ((corpusItemFreqs <$>) . <$>) . (flip M.lookup) corpus

ngramSets :: Corpus -> Map Text (Set NGram)
ngramSets Corpus{..} = M.map (S.fromList . join . map corpusItemNGrams) corpusItems

corpusFromItem :: CorpusItem -> Corpus
corpusFromItem c@CorpusItem{..} =
  Corpus $ M.fromList [(corpusItemName, [c])]

genFreqMap :: [NGram] -> FreqMap
genFreqMap doc =
  FreqMap $ foldl' (\acc nGram -> M.insertWith (+) nGram 1 acc) M.empty doc

mkCorpusItem :: (Monad m) => Document -> DocumentReader m CorpusItem
mkCorpusItem Document{..} = ask >>= (\settings ->
  let
    newText = genNMGrams (minGrams settings) (maxGrams settings) . cleanText $ docText
    freqs = genFreqMap newText
  in return $ CorpusItem docName docText newText freqs)

addToCorpus :: (Monad m) => Corpus -> Document -> DocumentReader m Corpus
addToCorpus corpus doc = mkCorpusItem doc >>= (\item ->
  return $ corpus <> (corpusFromItem item))

genCorpus :: (Monad m) => [Document] -> DocumentReader m Corpus
genCorpus = foldM addToCorpus mempty
