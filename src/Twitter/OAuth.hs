{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Twitter.OAuth where

import Crypto.Hash (hmacAlg, HMAC(..), SHA1(..))
import Data.Byteable (toBytes)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as BS64
import Network.HTTP.Types.URI (urlEncode)
import Text.StringRandom (stringRandomIO)
import Twitter.Types

data AccessLevel = Read | ReadWrite | ReadWriteDM
  deriving (Show, Eq)

fromString :: String -> Maybe AccessLevel
fromString "read" = Just Read
fromString "read-write" = Just ReadWrite
fromString "read-write-directmessages" = Just ReadWriteDM
fromString _ = Nothing

data OAuthParams = OAuthParams { oauthConsumerKey :: ByteString
                               , oauthNonce :: ByteString
                               , oauthSignature :: Maybe ByteString
                               , oauthSignatureMethod :: ByteString
                               , oauthTimestamp :: Integer
                               , oauthToken :: ByteString
                               , oauthVersion :: ByteString
                               }
  deriving (Show, Eq)

makeFullRequest :: Token -> Request -> Secret -> IO (ByteString, ByteString)
makeFullRequest token req sec = do
  time <- getPOSIXTime
  nonce <- stringRandomIO "[a-zA-Z0-9]{32}"
  return $ makePureRequest time nonce token req sec

makePureRequest :: NominalDiffTime -> Text -> Token -> Request -> Secret -> (ByteString, ByteString)
makePureRequest time nonce token req sec =
  let oauthToken = makePureOAuthParams time nonce token
      oauthParams = makeAuthorizationHeader req sec oauthToken
      reqParams = map encodePair $ requestParams req
      paramStr = BS.intercalate "&" reqParams
  in (paramStr, oauthParams)


makePureOAuthString :: NominalDiffTime -> Text -> Token -> Request -> Secret -> ByteString
makePureOAuthString time nonce token req sec =
  let oap = makePureOAuthParams time nonce token
  in makeAuthorizationHeader req sec oap

-- makeOAuthString :: Token -> Request -> Secret -> IO ByteString
-- makeOAuthString token req sec = do
  -- oap <- makeOAuthParams token
  -- return $ makeAuthorizationHeader req sec oap

makePureOAuthParams :: NominalDiffTime -> Text -> Token -> OAuthParams
makePureOAuthParams time nonce (Token{..}) =
  OAuthParams { oauthConsumerKey = tokenConsumer
              , oauthNonce = encodeUtf8 nonce
              , oauthSignature = Nothing
              , oauthSignatureMethod = "HMAC-SHA1"
              , oauthTimestamp = round time
              , oauthToken = fromMaybe "" tokenAccess
              , oauthVersion = "1.0"
              }

-- makeOAuthParams :: Token -> IO OAuthParams
-- makeOAuthParams token = do
  -- time <- getPOSIXTime
  -- nonce <- stringRandomIO "[a-zA-Z0-9]{32}"
  -- return $ makePureOAuthParams (round time) (encodeUtf8 nonce) token

oAuthToParams :: OAuthParams -> [(ByteString, ByteString)]
oAuthToParams (OAuthParams{..}) =
  let base = [ ("oauth_consumer_key", oauthConsumerKey)
             , ("oauth_nonce", oauthNonce)
             , ("oauth_signature_method", oauthSignatureMethod)
             , ("oauth_timestamp", BSC.pack $ show oauthTimestamp)
             , ("oauth_token", oauthToken)
             , ("oauth_version", oauthVersion)
             ]
  in case oauthSignature of
    Just sig -> ("oauth_signature", sig) : base
    Nothing -> base

encodeParamsWithRequest :: Request -> OAuthParams -> ByteString
encodeParamsWithRequest (Request{..}) oauthParams =
  let unsigned = oauthParams { oauthSignature = Nothing }
      params = requestParams ++ (oAuthToParams unsigned)
  in percentEncode $ encodeParams params

signatureBaseString :: Request -> OAuthParams -> ByteString
signatureBaseString req@(Request{..}) oauthParams =
   let encodedParams = encodeParamsWithRequest req oauthParams
       (URL url) = requestURL
       encodedURL = percentEncode url
   in BS.concat [BSC.pack $ show requestMethod, "&", encodedURL, "&", encodedParams]

makeOAuthSignature :: Request -> Secret -> OAuthParams -> ByteString
makeOAuthSignature req secret oauthParams =
  let fullKey = signatureBaseString req oauthParams
  in BS64.encode . toBytes . hmaSign fullKey $ secret

signOAuthParams :: Request -> Secret -> OAuthParams -> OAuthParams
signOAuthParams req secret oauthParams =
  let signature = makeOAuthSignature req secret oauthParams
  in oauthParams { oauthSignature = Just signature }

makeSigningKey :: Secret -> ByteString
makeSigningKey (Secret{..}) = BS.concat [secretConsumer, "&", fromMaybe "" secretToken]

hmaSign :: ByteString -> Secret -> HMAC SHA1
hmaSign msg secret = hmacAlg SHA1 (makeSigningKey secret) msg

preparePair :: (ByteString, ByteString) -> ByteString
preparePair (k, v) = BS.concat [percentEncode k, "=\"", percentEncode v, "\""]

encodePair :: (ByteString, ByteString) -> ByteString
encodePair (k, v) = BS.concat [percentEncode k, "=", percentEncode v]

percentEncode :: ByteString -> ByteString
percentEncode = urlEncode True

encodeParams :: [(ByteString, ByteString)] -> ByteString
encodeParams allParams =
  let
    encoded = map encodePair allParams
    sorted = sort encoded
  in BS.intercalate "&" sorted

makeAuthorizationHeader :: Request -> Secret -> OAuthParams -> ByteString
makeAuthorizationHeader req secret oap =
  let
    signed = signOAuthParams req secret oap
    sorted = sort . map preparePair . oAuthToParams $ signed
    paramStr = BS.intercalate ", " sorted
  in BS.concat ["OAuth ", paramStr]
