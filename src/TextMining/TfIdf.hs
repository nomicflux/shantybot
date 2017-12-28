{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TextMining.TfIdf where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

import TextMining.DocumentReader
import TextMining.NGram
import TextMining.Corpus

newtype ProbMap = ProbMap (Map NGram Double)
  deriving (Show, Eq)

newtype TfIdf = TfIdf { tfidfFreqs :: Map Text ProbMap }
  deriving (Show, Eq)

getTf :: CorpusItem -> ProbMap
getTf item = ProbMap $ normalize freqs
  where
    (FreqMap freqs) = corpusItemFreqs item
    total = fromIntegral $ M.foldl' (+) 0 freqs
    normalize = M.map ((/ total) . fromIntegral)

getIdf :: Corpus -> CorpusItem -> ProbMap
getIdf refCorpus genCorpus =
  ProbMap getTotal
  where
    refNGrams :: Map Text (Set NGram)
    refNGrams = ngramSets refCorpus

    genNGrams :: [NGram]
    genNGrams = S.toList $ M.foldl' (flip S.insert) S.empty $ ngramSets genCorpus

    numTexts :: Double
    numTexts = fromIntegral . length . M.keys $ refNGrams

    getIdfWord :: Int -> Double
    getIdfWord extant = numTexts / fromIntegral (1 + extant)

    getTotal :: NGram -> Double
    getTotal ngram =
      M.foldl' (+) 0 $ M.map (\v -> if S.member ngram v then 1 else 0) refNGrams

    getProbMap :: Map NGram Double
    getProbMap = M.fromList (\w -> (w, getTotal w)) genNGrams

genTfIdf :: Corpus -> TfIdf
genTfIdf corpus@(Corpus m n freqMaps) =
  TfIdf m n $ M.intersectionWith mergeTfMap tfs idf
  where
    tfs :: Map Text TfMap
    tfs = M.map getTf freqMaps
    idf :: Map Text TfMap
    idf = getIdf corpus
    mergeTfMap :: TfMap -> TfMap -> TfMap
    mergeTfMap (TfMap m1) (TfMap m2) = TfMap $ M.intersectionWith (*) m1 m2

genTfIdfFromDocs :: Int -> Int -> [Document] -> TfIdf
genTfIdfFromDocs m n d = genTfIdf $ genCorpus m n (mkCleanedDoc <$> d)

getTfFromDoc :: FreqMap -> TfMap
getTfFromDoc = getTf

getIdfFromDoc :: TfIdf -> FreqMap -> TfMap
getIdfFromDoc (TfIdf _ _ tfidf) = getIdfForDoc freqMaps
  where
    freqMaps = M.map (\(TfMap m) -> m) tfidf

getTfIdfFromCDoc :: Int -> Int -> TfIdf -> CleanedDoc -> TfMap
getTfIdfFromCDoc m n tfidf doc = TfMap $ M.intersectionWith (*) tf idf
  where
    freqMap = genFreqMap m n doc
    (TfMap tf) = getTfFromDoc freqMap
    (TfMap idf) = getIdfFromDoc tfidf freqMap

getTfIdfFromDoc :: Int -> Int -> TfIdf -> Document -> TfMap
getTfIdfFromDoc m n tfidf doc = TfMap $ M.intersectionWith (*) tf idf
  where
    freqMap = genFreqMap m n $ mkCleanedDoc doc
    (TfMap tf) = getTfFromDoc freqMap
    (TfMap idf) = getIdfFromDoc tfidf freqMap

cosineSimilarity :: TfMap -> TfMap -> Double
cosineSimilarity (TfMap m1) (TfMap m2) =
  (sum . fmap snd . M.toList . M.intersectionWith (*) m1 $ m2) / totalSize
  where
    size = M.foldl' (+) 0.0
    totalSize = size m1 * size m2

getSimilarities :: TfMap -> TfIdf -> Map Text Double
getSimilarities phrase tfidf = M.map (cosineSimilarity phrase) (tfidfFreqs tfidf)

bestMatch :: TfMap -> TfIdf -> Maybe Text
bestMatch phrase tfidf = fst $ getMatch (M.toList matches)
  where
    matches = getSimilarities phrase tfidf
    getMatch :: [(Text, Double)] -> (Maybe Text, Double)
    getMatch =
      foldl (\acc@(_, accV) (k, v) ->
                if v > accV then (Just k, v) else acc) (Nothing, 0.0)

phraseVals :: Text -> TfIdf -> [(Text, Double)]
phraseVals phrase t@(TfIdf m n _) =
  sortOn (negate . snd) $ M.toList $ getSimilarities (getTfIdfFromCDoc m n t $ mkCleanedDocFromText "" phrase) t

matchPhrase :: Text -> TfIdf -> Maybe Text
matchPhrase phrase t@(TfIdf m n _) =
  bestMatch (getTfIdfFromCDoc m n t $ mkCleanedDocFromText "" phrase) t
