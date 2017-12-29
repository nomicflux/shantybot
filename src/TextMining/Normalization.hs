{-# LANGUAGE OverloadedStrings #-}

module TextMining.Normalization where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import TextMining.Document (Line(..))
import TextMining.NGram (NGram(..))

punctuation :: String
punctuation = ",.?!-:;\"\'"

singularize :: Text -> Text
singularize word
  | T.takeEnd 3 word == "ies" = T.dropEnd 3 word <> "y"
  | T.takeEnd 3 word == "ses" = T.dropEnd 2 word
  | T.takeEnd 1 word == "s" = T.dropEnd 1 word
  | otherwise = word

decontraction :: Text -> Text
decontraction word
  | last2 == "'s" = T.dropEnd 2 word <> " is"
  | last2 == "'m" = T.dropEnd 2 word <> " am"
  | last3 == "'ll" = T.dropEnd 3 word <> " will"
  | last3 == "'ve" = T.dropEnd 3 word <> " have"
  | last3 == "'nt" = T.dropEnd 3 word <> " not"
  | last3 == "'re" = T.dropEnd 3 word <> " are"
  | otherwise = word
  where
    last2 = T.takeEnd 2 word
    last3 = T.takeEnd 3 word

depunctuate :: Text -> Text
depunctuate = T.filter (not . (`elem` punctuation))

normalizeWord :: Text -> Text
normalizeWord = singularize . depunctuate . decontraction . T.strip

cleanLine :: Line -> [NGram]
cleanLine = (NGram <$>) . filter (/= "") . (normalizeWord <$>) . T.words . T.toLower . lineText

cleanText :: [Line] -> [NGram]
cleanText = (>>= cleanLine)
