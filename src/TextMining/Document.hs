{-# LANGUAGE OverloadedStrings #-}
module TextMining.Document where

import Control.Monad (msum)
import Data.List (tails, length, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

data Document = Document { docName :: Text
                         , ext :: [Text]
                         }
  deriving (Show, Eq)

newtype FreqMap = FreqMap (Map Text Int)
  deriving (Show, Eq)

data Corpus = Corpus { corpusMinGrams :: Int
                     , corpusMaxGrams :: Int
                     , corpusFreqs :: (Map Text FreqMap)
                     }
  deriving (Show, Eq)

newtype TfMap = TfMap (Map Text Double)
  deriving (Show, Eq)

data TfIdf = TfIdf { tfidfMinGrams :: Int
                   , tfidfMaxGrams :: Int
                   , tfidfFreqs :: (Map Text TfMap)
                   }
  deriving (Show, Eq)

punctuation :: String
punctuation = ",.?!-:;\"\'"

cleanText :: Text -> Text
cleanText = T.toLower . T.filter (not . (`elem` punctuation))

mkDocument :: Text -> Text -> Document
mkDocument name text = Document name $ T.words $ cleanText text

comboText :: Text
comboText = " "

mkNGram :: Int -> [Text] -> Maybe Text
mkNGram n doc
  | length doc == n = Just . T.tail $ foldl' (\acc word -> acc <> comboText <> word) "" doc
  | otherwise = Nothing

genNGrams :: [Text] -> Int -> [Text]
genNGrams doc n = mapMaybe (mkNGram n) (take n <$> tails doc)

genNMGrams :: Document -> Int -> Int -> Document
genNMGrams (Document name doc) minGrams maxGrams = Document name . msum . map (genNGrams doc) $ [minGrams .. maxGrams]

genFreqMap :: Int -> Int -> Document -> FreqMap
genFreqMap minGrams maxGrams doc = FreqMap $ foldl' (\acc nGram -> M.insertWith (+) nGram 1 acc) M.empty nmGrams
  where
    (Document _ nmGrams) = genNMGrams doc minGrams maxGrams

genCorpus :: Int -> Int -> [Document] -> Corpus
genCorpus minGrams maxGrams docs = Corpus minGrams maxGrams $ M.fromList freqs
  where
    gen = genFreqMap minGrams maxGrams
    freqs = fmap (\doc@(Document name _) -> (name, gen doc)) docs

addToCorpus :: Corpus -> Text -> Text -> Corpus
addToCorpus (Corpus m n docs) name text = Corpus m n newDocs
  where
    newDoc = mkDocument name text
    newFreqs = genFreqMap m n newDoc
    newDocs = M.insert name newFreqs docs

getTf :: FreqMap -> TfMap
getTf (FreqMap freqs) = TfMap $ normalize freqs
  where
    total = fromIntegral $ M.foldl (+) 0 freqs
    normalize = M.map ((/ total) . fromIntegral)

getIdf :: Corpus -> Map Text TfMap
getIdf (Corpus _ _ freqMaps) = M.mapWithKey transform freqMaps
  where
    numTexts :: Double
    numTexts = fromIntegral $ length $ M.keys freqMaps
    allKeys :: [Text]
    allKeys = foldl (\acc (FreqMap m) -> acc ++ M.keys m) [] freqMaps
    getTotal :: Text -> Int
    getTotal word = foldl (\acc (FreqMap m) -> acc + (maybe 0 (const 1) (M.lookup word m))) 0 freqMaps
    getIdfWord :: Text -> Int -> Double
    getIdfWord word _ =
      let bottom = getTotal word
      in log $ numTexts / (fromIntegral $ 1 + bottom)
    transform :: Text -> FreqMap -> TfMap
    transform docName f@(FreqMap freqs) =
      TfMap $ M.mapWithKey getIdfWord freqs

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
genTfIdfFromDocs m n d = genTfIdf $ genCorpus m n d

getTfFromDoc :: Int -> Int -> Document -> TfMap
getTfFromDoc m n d = getTf $ genFreqMap m n d

cosineSimilarity :: TfMap -> TfMap -> Double
cosineSimilarity (TfMap m1) (TfMap m2) =
  sum . fmap snd . M.toList . M.intersectionWith (*) m1 $ m2

bestMatch :: TfMap -> TfIdf -> Maybe Text
bestMatch phrase tfidf = fst <$> getMatch (M.toList matches)
  where
    matches = M.map (cosineSimilarity phrase) (tfidfFreqs tfidf)
    getMatch [] = Nothing
    getMatch ((doc, rating) : rst) =
      Just $ foldl (\acc@(_, accV) nxt@(_, v) ->
                      if v > accV then nxt else acc) (doc, rating) rst

matchPhrase :: Text -> TfIdf -> Maybe Text
matchPhrase phrase t@(TfIdf m n _) =
  bestMatch (getTfFromDoc m n $ mkDocument "" phrase) t
