{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import OAuthSpec
import CorpusSpec

main :: IO ()
main = hspec $ do
  oauthTests
  corpusTests
