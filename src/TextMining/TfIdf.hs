{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TextMining.TfIdf where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import TextMining.Document (ToDocument(..))
import TextMining.DocumentReader
import TextMining.NGram
import TextMining.Corpus

newtype ProbMap = ProbMap { getProbMap :: Map NGram Double }
  deriving (Show, Eq)

data TfIdf = TfIdf { tfs :: Map Text ProbMap
                   , idf :: ProbMap
                   }
  deriving (Show, Eq)

getTf :: [CorpusItem a] -> ProbMap
getTf items = ProbMap $ normalize freqs
  where
    freqs = foldl' (\acc v -> M.unionWith (+) acc . getFreqs . corpusItemFreqs $ v) M.empty items
    total = fromIntegral $ M.foldl' (+) 0 freqs
    normalize = M.map ((/ total) . fromIntegral)

getIdf :: Corpus a -> ProbMap
getIdf refCorpus = ProbMap genProbMap
  where
    refNGrams :: Map Text (Set NGram)
    refNGrams = ngramSets refCorpus

    genGrams :: [NGram]
    genGrams = S.toList $ M.foldl' (flip S.union) S.empty refNGrams

    numTexts :: Double
    numTexts = fromIntegral . length . M.keys $ refNGrams

    getTotal :: NGram -> Double
    getTotal ngram =
      M.foldl' (+) 0 $ M.map (\v -> if S.member ngram v then 1 else 0) refNGrams

    genProbMap :: Map NGram Double
    genProbMap = M.fromList $ (\w -> (w, getTotal w / numTexts)) <$> genGrams

genTfIdf :: Corpus a -> TfIdf
genTfIdf corpus = TfIdf tfs' idf'
  where
    tfs' :: Map Text ProbMap
    tfs' = M.map getTf (corpusItems corpus)
    idf' :: ProbMap
    idf' = getIdf corpus

cosineSimilarity :: ProbMap -> ProbMap -> Double
cosineSimilarity (ProbMap m1) (ProbMap m2) =
  (sum . fmap snd . M.toList . M.intersectionWith (*) m1 $ m2)

getSimilarities :: ProbMap -> TfIdf -> Map Text Double
getSimilarities phrase tfidf = M.map (cosineSimilarity idfPhrase) (tfs tfidf)
  where
    idfPhrase :: ProbMap
    idfPhrase = ProbMap $ M.intersectionWith (\i w -> w * i * i) (getProbMap $ idf tfidf) (getProbMap phrase)

bestMatch :: CorpusItem a -> TfIdf -> Maybe Text
bestMatch phrase tfidf = fst $ getMatch (M.toList matches)
  where
    matches = getSimilarities (getTf [phrase]) tfidf
    getMatch :: [(Text, Double)] -> (Maybe Text, Double)
    getMatch =
      foldl (\acc@(_, accV) (k, v) ->
                if v > accV then (Just k, v) else acc) (Nothing, 0.0)

matchPhrase :: Monad m => Text -> TfIdf -> DocumentReader m (Maybe Text)
matchPhrase phrase tfidf = do
  corpusItem <- mkCorpusItem (toDocument phrase)
  return $ bestMatch corpusItem tfidf

matchInCorpus :: (Monad m, ToDocument a) => Corpus a -> Text -> TfIdf -> DocumentReader m [a]
matchInCorpus corpus phrase tfidf = do
  corpusItem <- mkCorpusItem (toDocument phrase)
  return $ maybe [] (structuredDocsFromCorpus corpus) (bestMatch corpusItem tfidf)
