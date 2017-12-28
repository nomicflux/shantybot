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
  | T.takeEnd 1 word == "s" = T.dropEnd 1 word
  | otherwise = word

decontraction :: Text -> Text
decontraction word
  | T.takeEnd 2 word == "'s" = T.dropEnd 2 word <> " is"
  | T.takeEnd 3 word == "'ll" = T.dropEnd 3 word <> " will"
  | T.takeEnd 3 word == "'ve" = T.dropEnd 3 word <> " have"
  | T.takeEnd 3 word == "'nt" = T.dropEnd 3 word <> " not"
  | otherwise = word

depunctuate :: Text -> Text
depunctuate = T.filter (not . (`elem` punctuation))

normalizeWord :: Text -> Text
normalizeWord = singularize . depunctuate . decontraction . T.strip

cleanLine :: Line -> [NGram]
cleanLine = (NGram <$>) . filter (/= "") . (normalizeWord <$>) . T.words . T.toLower . lineText

cleanText :: [Line] -> [NGram]
cleanText = (>>= cleanLine)
