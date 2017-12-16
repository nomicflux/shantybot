{-# LANGUAGE OverloadedStrings #-}

module Twitter.Streaming where

import Pipes ((>->), runEffect)
import Pipes.HTTP
import qualified Pipes.ByteString as PB
import qualified Pipes.Aeson as PA
import Control.Monad (mapM_)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Twitter.Types
import Twitter.Tweet
import Twitter.OAuth (makeFullRequest)
import Twitter.Internal.AccessInfo
import Network.HTTP.Client (RequestBody(..))
import Network.HTTP.Types.Header (hAuthorization)

twitterURL :: String
twitterURL = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"
--twitterURL = "https://api.twitter.com/1.1/statuses/update.json"

lookAtTweets :: [Tweet] -> IO ()
lookAtTweets = mapM_ (putStrLn . show)

request :: IO ()
request = do
  url <- parseUrlThrow twitterURL
  ck <- consumerKey
  at <- accessToken
  cs <- consumerSecret
  as <- accessSecret
  let
    --met = POST
    --req = Request met (URL (pack twitterURL)) [("status", "Ahoy, world!"), ("include_entities", "true")]
    met = GET
    req = Request met (URL (pack twitterURL)) []
    token = Token ck (Just at)
    secret = Secret { secretConsumer = cs
                    , secretToken = Just as
                    }
  (paramString, headerString) <- makeFullRequest token req secret
  let headers = [(hAuthorization, headerString)]
  BSC.putStrLn headerString
  let
    postReq = url { method = pack $ show met
                  --, requestBody = RequestBodyLBS . fromStrict $ paramString
                  , queryString = paramString
                  }
    withAuth = postReq { requestHeaders = headers ++ requestHeaders postReq }
  manager <- newManager tlsManagerSettings
  withHTTP withAuth manager $ \resp -> do
    mres <- evalStateT PA.decode (responseBody resp) :: IO (Maybe (Either PA.DecodingError [Tweet]))
    let eres = fromMaybe (Right []) mres
        res = either (const []) id eres
    lookAtTweets res
