{-# LANGUAGE OverloadedStrings #-}

module CorpusSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import TextMining.Document

corpusTests :: Spec
corpusTests = do
  let testPhrase1 = "hello world" :: Text
      testPhrase2 = "the sky is the blue today,  is it is it not? " :: Text
      testWords1 = cleanText testPhrase1
      testWords2 = cleanText testPhrase2
      docName1 = "test1" :: Text
      docName2 = "test2" :: Text
      doc1 = Document docName1 testWords1
      doc2 = Document docName2 testWords2
  describe "Document.genNGram" $ do
    it "should generate simple words" $ do
      genNGrams testWords1 1 `shouldBe` testWords1
      genNGrams testWords2 1 `shouldBe` testWords2
    it "should create 2-grams" $ do
      genNGrams testWords1 2 `shouldBe` ["hello world"]
      genNGrams testWords2 2 `shouldBe` ["the sky", "sky is", "is the", "the blue", "blue today", "today is", "is it", "it is", "is it", "it not"]

  describe "Document.mkDocument" $ do
    it "should make a simple document" $ do
      mkDocument docName1 testPhrase1 `shouldBe` doc1
      mkDocument docName2 testPhrase2 `shouldBe` doc2

  describe "Document.genNMGrams" $ do
    let grams1a = genNGrams testWords1 1
        grams1b = genNGrams testWords2 1
        grams2a = genNGrams testWords1 2
        grams2b = genNGrams testWords2 2
    it "should generate 1-gram documents" $ do
      genNMGrams doc1 1 1 `shouldBe` Document docName1 grams1a
      genNMGrams doc2 1 1 `shouldBe` Document docName2 grams1b
    it "should generate 2-gram documents" $ do
      genNMGrams doc1 2 2 `shouldBe` Document docName1 grams2a
      genNMGrams doc2 2 2 `shouldBe` Document docName2 grams2b
    it "should generate 1-2-gram documents" $ do
      genNMGrams doc1 1 2 `shouldBe` Document docName1 (grams1a ++ grams2a)
      genNMGrams doc2 1 2 `shouldBe` Document docName2 (grams1b ++ grams2b)

  describe "Document.genFreqMap" $ do
    let map1a = FreqMap $ M.fromList [("hello", 1), ("world", 1)]
        map1b = FreqMap $ M.fromList [("the", 1), ("sky", 1), ("is", 3), ("the", 2), ("blue", 1), ("today", 1), ("it", 2), ("not", 1)]
        map2a = FreqMap $ M.fromList [("hello world", 1)]
        map2b = FreqMap $ M.fromList [("the", 1), ("sky", 1), ("is", 3), ("the", 2), ("blue", 1), ("today", 1), ("it", 2), ("not", 1), ("the sky", 1), ("sky is", 1), ("is the", 1), ("the blue", 1), ("blue today", 1), ("today is", 1), ("is it", 2), ("it is", 1), ("it not", 1)]
    it "should generate 1-gram freqMaps" $ do
      genFreqMap 1 1 doc1 `shouldBe` map1a
      genFreqMap 1 1 doc2 `shouldBe` map1b
    it "should generate 2-gram freqMaps" $ do
      genFreqMap 2 2 doc1 `shouldBe` map2a
    it "should generate 1-2-gram freqMaps" $ do
      genFreqMap 1 2 doc2 `shouldBe` map2b

  describe "Document.genCorpus" $ do
    let
      map1a = FreqMap $ M.fromList [("hello", 1), ("world", 1)]
      map1b = FreqMap $ M.fromList [("the", 1), ("sky", 1), ("is", 3), ("the", 2), ("blue", 1), ("today", 1), ("it", 2), ("not", 1)]
      corpus = Corpus 1 1 $ M.fromList [(docName1, map1a), (docName2, map1b)]
    it "should generate a corpus from documents" $ do
      genCorpus 1 1 [doc1, doc2] `shouldBe` corpus
