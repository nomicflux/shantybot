{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module TextMining.Document where

import Control.Monad (msum)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), ToJSON, toJSON, object, (.=))
import Data.List (tails, length, foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

data Document = Document { docName :: Text
                         , docText :: Text
                         }

instance FromJSON Document where
  parseJSON = withObject "DocObject" $ \o -> do
    docName <- o .: "name"
    docText <- o .: "text"
    return Document{..}

instance ToJSON Document where
  toJSON Document{..} = object [ "name" .= docName
                               , "text" .= docText
                               ]

type Documents = Map Text Document

data CleanedDoc = CleanedDoc { cdocName :: Text
                             , cdocText :: [Text]
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

cleanText :: Text -> [Text]
cleanText = filter (/= "") . (T.strip <$>) . T.words . T.toLower . T.filter (not . (`elem` punctuation))

mkCleanedDoc :: Document -> CleanedDoc
mkCleanedDoc Document{..} = CleanedDoc docName $ cleanText docText

mkCleanedDocFromText :: Text -> Text -> CleanedDoc
mkCleanedDocFromText = (mkCleanedDoc .) . mkDocument

mkDocument :: Text -> Text -> Document
mkDocument name text = Document name text

comboText :: Text
comboText = " "

mkNGram :: Int -> [Text] -> Maybe Text
mkNGram n doc
  | length doc == n = Just . T.tail $ foldl' (\acc word -> acc <> comboText <> word) "" doc
  | otherwise = Nothing

genNGrams :: [Text] -> Int -> [Text]
genNGrams doc n = mapMaybe (mkNGram n) (take n <$> tails doc)

genNMGrams :: CleanedDoc -> Int -> Int -> CleanedDoc
genNMGrams (CleanedDoc name doc) minGrams maxGrams = CleanedDoc name . msum . map (genNGrams doc) $ [minGrams .. maxGrams]

genFreqMap :: Int -> Int -> CleanedDoc -> FreqMap
genFreqMap minGrams maxGrams doc = FreqMap $ foldl' (\acc nGram -> M.insertWith (+) nGram 1 acc) M.empty nmGrams
  where
    (CleanedDoc _ nmGrams) = genNMGrams doc minGrams maxGrams

genCorpus :: Int -> Int -> [CleanedDoc] -> Corpus
genCorpus minGrams maxGrams docs = Corpus minGrams maxGrams $ M.fromList freqs
  where
    gen = genFreqMap minGrams maxGrams
    freqs = fmap (\doc@(CleanedDoc name _) -> (name, gen doc)) docs

getTf :: FreqMap -> TfMap
getTf (FreqMap freqs) = TfMap $ normalize freqs
  where
    total = fromIntegral $ M.foldl' (+) 0 freqs
    normalize = M.map ((/ total) . fromIntegral)

getIdfForDoc :: Map Text (Map Text a) -> FreqMap -> TfMap
getIdfForDoc freqMaps (FreqMap freqs) = TfMap $ M.mapWithKey getIdfWord freqs
  where
    numTexts :: Double
    numTexts = fromIntegral $ length $ M.keys freqMaps

    allKeys :: [Text]
    allKeys = foldl (\acc m -> acc ++ M.keys m) [] freqMaps

    getTotal :: Text -> Int
    getTotal word = foldl (\acc m -> acc + (maybe 0 (const 1) (M.lookup word m))) 0 freqMaps

    getIdfWord :: Text -> Int -> Double
    getIdfWord word _ =
      let bottom = getTotal word
      in numTexts / (fromIntegral $ 1 + bottom)

getIdf :: Corpus -> Map Text TfMap
getIdf (Corpus _ _ docs) = M.map (getIdfForDoc freqMaps) docs
  where
    freqMaps = M.map (\(FreqMap m) -> m) docs

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
getTfFromDoc d = getTf d

getIdfFromDoc :: TfIdf -> FreqMap -> TfMap
getIdfFromDoc (TfIdf _ _ tfidf) d = getIdfForDoc freqMaps d
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
    totalSize = (size m1) * (size m2)

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
