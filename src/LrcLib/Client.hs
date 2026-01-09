{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for calling LRCLIB API (<https://lrclib.net/docs>)
module LrcLib.Client
  ( module LrcLib.Types,
    runAPI,
    runDefaultAPI,
    getLyrics,
    getCachedLyrics,
    getLyricsById,
    searchLyrics,
    publish',
    publish,
    requestChallenge,
    solveChallenge,
  )
where

import Control.Monad.Reader (runReaderT)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack
import LrcLib.HTTP as HTTP
import LrcLib.Types
import Network.HTTP.Client
  ( Request,
    Response (responseBody, responseStatus),
    parseRequest_,
  )
import Network.HTTP.Types (Status (statusCode))
import Prelude hiding (id)

decode :: (A.FromJSON a, HasCallStack) => ByteStringL -> a
decode x = case A.eitherDecode x of
  Left err -> error $ "Error while decoding JSON: " <> err
  Right y -> y

-- | Run API action with given API url
runAPI :: Request -> API a -> IO a
runAPI url f = runReaderT f url

-- | Run API action ('runAPI') with default API url
--
-- >>> runDefaultAPI $ getLyricsById 1337
-- Right Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
runDefaultAPI :: API a -> IO a
runDefaultAPI = runAPI $ parseRequest_ "https://lrclib.net/api"

get' :: ByteString -> Text -> Text -> Text -> Integer -> API (Either GetError TrackData)
get' subpath track artist album duration = do
  resp <- HTTP.get subpath query
  case resp.responseStatus.statusCode of
    404 -> pure $ Left NotFound
    200 -> pure $ Right $ decode resp.responseBody
    s -> error $ "Unexpected status code on get endpoint: " <> show s
  where
    query =
      [ ("track_name", track),
        ("artist_name", artist),
        ("album_name", album),
        ("duration", T.show duration)
      ]

getLyrics,
  getCachedLyrics ::
    Text -> Text -> Text -> Integer -> API (Either GetError TrackData)

-- | Get lyrics by track name, artist, album and duration
-- Calls @\/api\/get@
--
-- >>> runDefaultAPI $ getLyrics "ThisTrackDoesntExist" "UnknownArtist" "UnknownAlbum" 1337
-- Left NotFound
--
-- >>> runDefaultAPI $ getLyrics "Speak Français" "Ellis feat. NOËP" "Speak Français" 201
-- Right Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
getLyrics = get' "/get"

-- | The same as 'getLyrics', but for cached lyrics.
-- Calls @\/api\/get-cached@
getCachedLyrics = get' "/get-cached"

-- | Get lyrics by id
-- Calls @\/api\/get\/\<id\>@
--
-- >>> runDefaultAPI $ getLyricsById 1337
-- Right Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
getLyricsById :: (HasCallStack) => Integer -> API (Either GetError TrackData)
getLyricsById id = do
  resp <- HTTP.get ("/get/" <> BC.pack (show id)) []
  case resp.responseStatus.statusCode of
    404 -> pure $ Left NotFound
    200 -> pure $ Right $ decode resp.responseBody
    _ -> error "Unexpected status code on get endpoint"

-- | Search lyrics by either text query or track-artist-album
-- Calls @\/api\/search@
--
-- >>> head <$> (runDefaultAPI $ searchLyrics $ TextQuery "Speak Français")
-- Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
searchLyrics :: SearchQuery -> API SearchResponse
searchLyrics q = do
  resp <- HTTP.get "/search" query
  pure $ decode resp.responseBody
  where
    query = case q of
      TextQuery t -> [("q", t)]
      TrackQuery {..} ->
        [("track_name", queryName)]
          ++ [("artist_name", artist) | Just artist <- pure queryArtist]
          ++ [("album_name", album) | Just album <- pure queryAlbum]

-- | Publish Lyrics ('publish') without requesting and solving challenge
-- Calls @\/api\/publish@
publish' :: (HasCallStack) => PublishToken -> PublishRequest -> API (Either PublishError ())
publish' token request = do
  resp <-
    HTTP.post "/publish" [("X-Publish-Token", encodeUtf8 token)] $
      A.encode request
  case resp.responseStatus.statusCode of
    400 -> pure $ Left IncorrectToken
    201 -> pure $ Right ()
    s -> error $ "Unexpected status code on publish endpoint: " <> show s

-- | Request proof-of-work challenge for publish
-- Calls @\/api\/request-challenge@
--
-- >>> runDefaultAPI requestChallenge
-- Challenge {prefix = "BVNC...XoVu1H", target = "000000FF0...00000"}
requestChallenge :: API Challenge
requestChallenge = do
  resp <- HTTP.post "/request-challenge" [] ""
  pure $ decode resp.responseBody

-- | Solve proof-of-work challenge for publish
-- Solution is a one-time publish token
solveChallenge :: Challenge -> Text
solveChallenge Challenge {prefix, target} = go 0
  where
    prefix' = encodeUtf8 prefix
    target' = fromRight (error "Can't decode target from crypto-challenge") $ B16.decode $ encodeUtf8 target
    go :: Int -> Text
    go n | hash (prefix' <> BC.pack (show n)) < target' = prefix <> ":" <> T.show n
    go n = go (n + 1)

-- | Publish Lyrics (request and solve challenge)
-- Calls @\/api\/publish@
publish :: (HasCallStack) => PublishRequest -> API (Either PublishError ())
publish r = do
  c <- requestChallenge
  publish' (solveChallenge c) r
