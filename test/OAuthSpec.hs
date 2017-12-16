{-# LANGUAGE OverloadedStrings #-}

module OAuthSpec where

import Test.Hspec

import qualified Data.ByteString as BS

import Twitter.OAuth
import Twitter.Types

oauthTests :: Spec
oauthTests = do
  describe "OAuth.percentEncode" $ do
    it "should encode spaces" $ do
      percentEncode "hello world" `shouldBe` "hello%20world"
    it "should encode punctuation" $ do
      percentEncode "tnnArxj06cWHq44gCs1OSKk/jLY=" `shouldBe` "tnnArxj06cWHq44gCs1OSKk%2FjLY%3D"
    it "should encode a phrase" $ do
      percentEncode "Hello Ladies + Gentlemen, a signed OAuth request!"
        `shouldBe` "Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21"

  describe "OAuth.encodePair" $ do
    it "should encode pairs not needing encoding" $ do
      encodePair ("oauth_token", "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb")
        `shouldBe` "oauth_token=370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
    it "should encode pairs needing encoding" $ do
      encodePair ("status", "Hello Ladies + Gentlemen, a signed OAuth request!")
        `shouldBe` "status=Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21"

  describe "OAuth.encodeParams" $ do
    let
      params1 = ("oauth_token", "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb")
      params2 = ("status", "Hello Ladies + Gentlemen, a signed OAuth request!")
      response = "oauth_token=370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb&status=Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21"
    it "should encode params" $ do
      encodeParams [params1, params2] `shouldBe` response
    it "should sort correctly" $ do
      encodeParams [params2, params1] `shouldBe` response

  describe "OAuth.makeOAuthParams" $ do
    let token = Token "abc" (Just "123")
        oauthParams = makePureOAuthParams 0 "nonce" token
    it "should correctly make params" $ do
      (oauthConsumerKey oauthParams) `shouldBe` (tokenConsumer token)
      (oauthSignature oauthParams) `shouldBe` Nothing
      (Just $ oauthToken oauthParams) `shouldBe` (tokenAccess token)

  describe "OAuth.oAuthToParams encoded" $ do
    let token = Token "xvz1evFS4wEEPTGEFPHBog" (Just "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb")
        oauthParams = oAuthToParams $ makePureOAuthParams 1318622958 "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" token
        encodedParams = encodeParams oauthParams
    it "should encode params correctly" $ do
      encodedParams `shouldBe` "oauth_consumer_key=xvz1evFS4wEEPTGEFPHBog&oauth_nonce=kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg&oauth_signature_method=HMAC-SHA1&oauth_timestamp=1318622958&oauth_token=370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb&oauth_version=1.0"

  describe "OAuth.encodeParamsWithRequest" $ do
    let token = Token "xvz1evFS4wEEPTGEFPHBog" (Just "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb")
        oauthParams = makePureOAuthParams 1318622958 "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" token
        request = Request POST
                          (URL "https://api.twitter.com/1.1/statuses/update.json")
                          [ ("include_entities", "true")
                          , ("status", "Hello Ladies + Gentlemen, a signed OAuth request!")
                          ]
        encodedParams = encodeParamsWithRequest request oauthParams
        response = "include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521"
    it "should correctly apply a request when encoding params" $ do
      encodedParams `shouldBe` response

  describe "OAuth.signatureBaseString" $ do
    let token = Token "xvz1evFS4wEEPTGEFPHBog" (Just "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb")
        oauthParams = makePureOAuthParams 1318622958 "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" token
        request = Request POST
                          (URL "https://api.twitter.com/1.1/statuses/update.json")
                          [ ("include_entities", "true")
                          , ("status", "Hello Ladies + Gentlemen, a signed OAuth request!")
                          ]
        baseString = signatureBaseString request oauthParams
        response = "POST&https%3A%2F%2Fapi.twitter.com%2F1.1%2Fstatuses%2Fupdate.json&include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521"
    it "should correctly apply a request when encoding params" $ do
      baseString `shouldBe` response

  describe "OAuth.makeSigningKey" $ do
    let secret = Secret "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
                        (Just "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE")
        response = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE"
    it "should correctly concatenate secret" $ do
      makeSigningKey secret `shouldBe` response

  describe "OAuth.makeOAuthSignature" $ do
    let token = Token "xvz1evFS4wEEPTGEFPHBog" (Just "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb")
        oauthParams = makePureOAuthParams 1318622958 "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg" token
        request = Request POST
                          (URL "https://api.twitter.com/1.1/statuses/update.json")
                          [ ("include_entities", "true")
                          , ("status", "Hello Ladies + Gentlemen, a signed OAuth request!")
                          ]
        secret = Secret "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
                        (Just "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE")
        signature = makeOAuthSignature request secret oauthParams
        response = "hCtSmYh+iHYCEqBWrE7C7hYmtUk="
    it "should correctly apply a request when encoding params" $ do
      signature `shouldBe` response

  describe "OAuth.makePureRequest" $ do
    let consumerKey = "xvz1evFS4wEEPTGEFPHBog"
        accessToken = "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
        token = Token consumerKey (Just accessToken)
        time = 1318622958
        nonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"
        request = Request POST
                          (URL "https://api.twitter.com/1.1/statuses/update.json")
                          [ ("include_entities", "true")
                          , ("status", "Hello Ladies + Gentlemen, a signed OAuth request!")
                          ]
        secret = Secret "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
                        (Just "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE")
        req = makePureRequest time nonce token request secret
        resp = BS.concat $ [ "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", "
                           , "oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", "
                           , "oauth_signature=\"hCtSmYh%2BiHYCEqBWrE7C7hYmtUk%3D\", "
                           , "oauth_signature_method=\"HMAC-SHA1\", "
                           , "oauth_timestamp=\"1318622958\", "
                           , "oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", "
                           , "oauth_version=\"1.0\""
                           ]
    it "should create the correct header" $ do
                   req `shouldBe` ("include_entities=true&status=Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21", resp)
