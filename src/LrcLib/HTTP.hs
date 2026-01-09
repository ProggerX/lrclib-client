{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LrcLib.HTTP where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LrcLib.Types
import Network.HTTP.Client
  ( Request (method, path, queryString, requestBody, requestHeaders),
    RequestBody (RequestBodyLBS),
    Response,
    httpLbs,
  )
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (RequestHeaders, renderSimpleQuery)

type ByteStringL = BSL.ByteString

post ::
  ByteString -> RequestHeaders -> ByteStringL -> API (Response ByteStringL)
post subpath requestHeaders body = do
  api <- ask
  let req =
        api
          { method = "POST",
            path = api.path <> subpath,
            requestHeaders,
            requestBody = RequestBodyLBS body
          }
  liftIO do
    man <- getGlobalManager
    httpLbs req man

get :: ByteString -> [(ByteString, Text)] -> API (Response ByteStringL)
get subpath query = do
  api <- ask
  let req =
        api
          { path = api.path <> subpath,
            queryString =
              renderSimpleQuery False $ map (second encodeUtf8) query
          }
  liftIO do
    man <- getGlobalManager
    httpLbs req man
