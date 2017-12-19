{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Twitter.Streaming where

import Pipes ((>->), runEffect, Pipe, await, yield, Producer, Consumer)
import Pipes.HTTP
import qualified Pipes.ByteString as PB
import qualified Pipes.Aeson as PA
import Control.Concurrent (threadDelay)
import qualified Control.Logging as L
import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT)
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy (fromStrict, ByteString)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime, NominalDiffTime, picosecondsToDiffTime, addUTCTime)
import qualified Data.Yaml as Yaml
import Network.HTTP.Client (RequestBody(..))
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.URI (urlEncode)

import qualified Twitter.Types as TT
import Twitter.Tweet
import Twitter.OAuth (makeFullRequest)
import Twitter.Internal.AccessInfo

getMentionsURL :: BSC.ByteString
getMentionsURL = "https://api.twitter.com/1.1/statuses/mentions_timeline.json"

postReplyURL :: BSC.ByteString
postReplyURL = "https://api.twitter.com/1.1/statuses/update.json"

doubleToNomDiffTime :: Double -> NominalDiffTime
doubleToNomDiffTime x =
  let d0 = ModifiedJulianDay 0
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime d0 (picosecondsToDiffTime $ floor (x/1e-12))
  in  diffUTCTime t1 t0

pauseUntil :: UTCTime -> IO ()
pauseUntil t = do
  now <- getCurrentTime
  case compare now t of
    LT -> threadDelay (truncate (diffUTCTime t now * 1000000))
    _  -> return ()

steadyCat :: (MonadIO m) => Double -> Pipe a a m r
steadyCat rate = do
  t0 <- liftIO getCurrentTime
  loop t0
  where
    dtUTC = doubleToNomDiffTime (1/rate)
    loop t =
      let t' = dtUTC `addUTCTime` t in do
        liftIO $ pauseUntil t'
        v <- await
        yield v
        loop t'

getConfig :: MonadIO m => m Config
getConfig = do
  ecfg <- liftIO $ Yaml.decodeFileEither "config/token-config.yaml"
  case ecfg of
    Left err -> do
      let msg = Yaml.prettyPrintParseException err
      liftIO $ L.warn' . T.pack $ msg
      fail msg
    Right val -> return $ val

getEndpoint :: MonadIO m => Config -> TT.Request -> m Request
getEndpoint (Config{..}) req@(TT.Request{..}) =  do
  let (TT.URL urlBS) =  requestURL
  url <- liftIO $ parseUrlThrow $ unpack urlBS
  (paramString, headerString) <- liftIO $ makeFullRequest configToken req configSecret
  let
    headers = [(hAuthorization, headerString)]
    httpReq = url { method = pack $ show requestMethod
                  , queryString = paramString
                  }
  return $ httpReq { requestHeaders = headers ++ requestHeaders httpReq }

getMentions :: MonadIO m => Config -> Maybe Text -> m Request
getMentions cfg lastId = do
  let
    msg :: Text
    msg = "Getting since " <> fromMaybe "<none>" lastId
  liftIO $ L.debug' msg
  getEndpoint cfg (TT.Request TT.GET (TT.URL getMentionsURL) (ct : sinceLastId))
  where
    ct = ("count", "200")
    sinceLastId = maybe [] (\i -> [("since_id", encodeUtf8 i)]) lastId

postReply :: MonadIO m => Config -> Tweet -> Text -> m Request
postReply cfg tweet msg = do
  liftIO . L.debug' . T.pack . show $ req
  getEndpoint cfg req
  where
    req = TT.Request TT.POST (TT.URL postReplyURL) [status, reply]
    status = ("status", encodeUtf8 $ msg)
    reply = ("in_reply_to_status_id", encodeUtf8 $ tweetId tweet)

loggerPipe :: MonadIO m => Consumer BSC.ByteString m ()
loggerPipe = do
  msg <- await
  liftIO $ L.debug (decodeUtf8 msg)

queryEndpoint :: (MonadIO m, FromJSON a) => Manager -> Request -> m [a]
queryEndpoint manager req =
  liftIO $ withHTTP req manager $ \resp -> do
    mres <- evalStateT PA.decode (responseBody resp)
    let eres = fromMaybe (Right []) mres
        res = either (const []) id eres
    return res

useEndpoint :: (MonadIO m) => Manager -> Request -> m ()
useEndpoint manager req =
  liftIO $ withHTTP req manager $ \resp -> do
    runEffect $ (responseBody resp) >-> loggerPipe
    return ()
