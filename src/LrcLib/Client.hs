{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LrcLib.Client
  ( module LrcLib.Types,
    getLyrics,
    getCachedLyrics,
    getLyricsById,
    requestChallenge,
    searchLyrics,
    publish',
    solveChallenge,
    publish,
    runAPI,
    runDefaultAPI,
  )
where

import Control.Lens ((&), (&~), (.=), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson qualified as A
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy (ByteString)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack
import LrcLib.Types
import Network.Wreq
import Prelude hiding (id)

decode :: (A.FromJSON a, HasCallStack) => ByteString -> a
decode x = case A.eitherDecode x of
  Left err -> error $ "Error while decoding JSON: " <> err
  Right y -> y

getUrl ::
  (HasCallStack) =>
  String -> Text -> Text -> Text -> Integer -> API (Either GetError TrackData)
getUrl url track artist album duration = do
  apiUrl <- ask
  resp <- liftIO $ getWith opts $ apiUrl <> url
  case resp ^. responseStatus . statusCode of
    404 -> pure $ Left NotFound
    200 -> pure $ Right $ decode (resp ^. responseBody)
    s -> error $ "Unexpected status code on get endpoint: " <> show s
  where
    opts =
      defaults &~ do
        param "track_name" .= [track]
        param "artist_name" .= [artist]
        param "album_name" .= [album]
        param "duration" .= [T.pack $ show duration]

getLyrics, getCachedLyrics ::
  Text -> Text -> Text -> Integer -> API (Either GetError TrackData)

-- | Get lyrics by track name, artist, album and duration
getLyrics = getUrl "/get"

-- | getLyrics, but for cached lyrics.
getCachedLyrics = getUrl "/get-cached"

-- | Get lyrics by id
getLyricsById :: (HasCallStack) => Integer -> API (Either GetError TrackData)
getLyricsById id = do
  url <- ask
  resp <- liftIO $ get $ url <> "/get/" <> show id
  case resp ^. responseStatus . statusCode of
    404 -> pure $ Left NotFound
    200 -> pure $ Right $ decode (resp ^. responseBody)
    _ -> error "Unexpected status code on get endpoint"

-- | Search lyrics by either text query or track-artist-album
searchLyrics :: SearchQuery -> API SearchResponse
searchLyrics q = do
  url <- ask
  resp <- liftIO $ getWith opts $ url <> "/search"
  pure $ decode (resp ^. responseBody)
  where
    opts =
      defaults &~ case q of
        TextQuery t -> param "q" .= [t]
        TrackQuery {..} -> do
          param "track_name" .= [queryName]
          param "artist_name" .= toList queryArtist
          param "album_name" .= toList queryAlbum

-- | Request proof-of-work challenge for publish
requestChallenge :: API Challenge
requestChallenge = do
  url <- ask
  resp <- liftIO $ post (url <> "/request-challenge") $ A.toJSON ()
  pure $ decode (resp ^. responseBody)

-- | Publish Lyrics (without requesting and solving challenge)
publish' ::
  (HasCallStack) => Text -> PublishRequest -> API (Either PublishError ())
publish' token request = do
  url <- ask
  res <- liftIO $ postWith opts (url <> "/publish") body
  case res ^. responseStatus . statusCode of
    400 -> pure $ Left IncorrectToken
    201 -> pure $ Right ()
    s -> error $ "Unexpected status code on publish endpoint: " <> show s
  where
    body = A.toJSON request
    opts = defaults & header "X-Publish-Token" .~ [encodeUtf8 token]

-- | Solve proof-of-work challenge for publish
solveChallenge :: Challenge -> Text
solveChallenge Challenge {prefix, target} = go 0
  where
    prefix' = encodeUtf8 prefix
    target' = fromRight (error "Can't decode target from crypto-challenge") $ B16.decode $ encodeUtf8 target
    go :: Int -> Text
    go n | hash (prefix' <> BC.pack (show n)) < target' = prefix <> ":" <> T.show n
    go n = go (n + 1)

-- | Publish Lyrics (request and solve challenge)
publish :: (HasCallStack) => PublishRequest -> API (Either PublishError ())
publish r = do
  c <- requestChallenge
  publish' (solveChallenge c) r

-- | Run API action with given API url
runAPI :: Url -> API a -> IO a
runAPI url f = runReaderT f url

-- | Run API action with default API url
runDefaultAPI :: API a -> IO a
runDefaultAPI = runAPI "http://lrclib.net/api"
