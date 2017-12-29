{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module TextMining.Corpus where

import Control.Monad (foldM, join)
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

newtype FreqMap = FreqMap { getFreqs :: Map NGram Int }
  deriving (Show, Eq)

instance Monoid FreqMap where
  mempty = FreqMap M.empty
  mappend (FreqMap m1) (FreqMap m2) = FreqMap $ M.unionWith (+) m1 m2

data CorpusItem a = CorpusItem { corpusItemName :: Text
                               , corpusItemText :: a
                               , corpusItemNGrams :: [NGram]
                               , corpusItemFreqs :: FreqMap
                               }
  deriving (Show, Eq)

data Corpus a = Corpus { corpusItems :: Map Text [CorpusItem a] }
  deriving (Show, Eq)

instance Monoid (Corpus a) where
  mempty = Corpus M.empty
  mappend (Corpus m1) (Corpus m2) = Corpus $ M.unionWith (++) m1 m2

documentFromCorpusItem :: ToDocument a => CorpusItem a -> Document
documentFromCorpusItem item = namedDocument (corpusItemName item) (toDocument . corpusItemText $ item)

docsFromCorpus :: ToDocument a => Corpus a -> Text -> [Document]
docsFromCorpus corpus name =
  maybe [] (documentFromCorpusItem <$>) $ M.lookup name (corpusItems corpus)

structuredDocsFromCorpus :: ToDocument a => Corpus a -> Text -> [a]
structuredDocsFromCorpus corpus name = maybe [] (corpusItemText <$>) $ M.lookup name (corpusItems corpus)

getTitles :: Corpus a -> [Text]
getTitles = M.keys . corpusItems

ngramSets :: Corpus a -> Map Text (Set NGram)
ngramSets Corpus{..} = M.map (S.fromList . join . map corpusItemNGrams) corpusItems

corpusFromItem :: CorpusItem a -> Corpus a
corpusFromItem c@CorpusItem{..} =
  Corpus $ M.fromList [(corpusItemName, [c])]

genFreqMap :: [NGram] -> FreqMap
genFreqMap doc =
  FreqMap $ foldl' (\acc nGram -> M.insertWith (+) nGram 1 acc) M.empty doc

mkCorpusItem :: (Monad m, ToDocument a) => a -> DocumentReader m (CorpusItem a)
mkCorpusItem doc = ask >>= (\settings ->
  let
    Document{..} = toDocument doc
    newText = genNMGrams (minGrams settings) (maxGrams settings) . cleanText $ docText
    freqs = genFreqMap newText
  in return $ CorpusItem docName doc newText freqs)

addToCorpus :: (Monad m, ToDocument a) => Corpus a -> a -> DocumentReader m (Corpus a)
addToCorpus corpus doc = mkCorpusItem doc >>= (\item ->
  return $ corpus <> (corpusFromItem item))

genCorpus :: (Monad m, ToDocument a) => [a] -> DocumentReader m (Corpus a)
genCorpus = foldM addToCorpus mempty
