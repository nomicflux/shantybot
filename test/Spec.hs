{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import OAuthSpec

main :: IO ()
main = hspec $ do
  oauthTests
