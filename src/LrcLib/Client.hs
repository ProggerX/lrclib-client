{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for calling LRCLIB API (<https://lrclib.net/docs>)
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

-- | Run API action with given API url
runAPI :: Url -> API a -> IO a
runAPI url f = runReaderT f url

-- | Run API action ('runAPI') with default API url
--
-- >>> runDefaultAPI $ getLyricsById 1337
-- OK Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
runDefaultAPI :: API a -> IO a
runDefaultAPI = runAPI "http://lrclib.net/api"

getUrl :: String -> Text -> Text -> Text -> Integer -> API GetResponse
getUrl url track artist album duration = do
  apiUrl <- ask
  resp <- liftIO $ getWith opts $ apiUrl <> url
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 -> pure $ OK $ decode (resp ^. responseBody)
    s -> error $ "Unexpected status code on get endpoint: " <> show s
  where
    opts =
      defaults &~ do
        checkResponse .= (Just $ \_ _ -> pure ())
        param "track_name" .= [track]
        param "artist_name" .= [artist]
        param "album_name" .= [album]
        param "duration" .= [T.pack $ show duration]

getLyrics, getCachedLyrics :: Text -> Text -> Text -> Integer -> API GetResponse

-- | Get lyrics by track name, artist, album and duration
-- Calls @\/api\/get@
--
-- >>> runDefaultAPI $ getLyrics "ThisTrackDoesntExist" "UnknownArtist" "UnknownAlbum" 1337
-- NotFound
--
-- >>> runDefaultAPI $ getLyrics "Speak Français" "Ellis feat. NOËP" "Speak Français" 201
-- OK Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
getLyrics = getUrl "/get"

-- | The same as 'getLyrics', but for cached lyrics.
-- Calls @\/api\/get-cached@
getCachedLyrics = getUrl "/get-cached"

-- | Get lyrics by id
-- Calls @\/api\/get\/\<id\>@
--
-- >>> runDefaultAPI $ getLyricsById 1337
-- OK Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
getLyricsById :: Integer -> API GetResponse
getLyricsById id' = do
  url <- ask
  resp <- liftIO $ getWith opts $ url <> "/get/" <> show id'
  case resp ^. responseStatus . statusCode of
    404 -> pure NotFound
    200 -> pure $ OK $ decode (resp ^. responseBody)
    _ -> error "Unexpected status code on get endpoint"
  where
    opts = defaults &~ checkResponse .= (Just $ \_ _ -> pure ())

-- | Search lyrics by either text query or track-artist-album
-- Calls @\/api\/search@
--
-- >>> head <$> (runDefaultAPI $ searchLyrics $ TextQuery "Speak Français")
-- Track, id: 1337, name: Speak Français, artist: Ellis feat. NOËP, album: Speak Français
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

-- | Publish Lyrics ('publish') without requesting and solving challenge
-- Calls @\/api\/publish@
publish' :: PublishToken -> PublishRequest -> API PublishResponse
publish' token request = do
  url <- ask
  res <- liftIO $ postWith opts (url <> "/publish") body
  case res ^. responseStatus . statusCode of
    400 -> pure IncorrectToken
    201 -> pure PublishOK
    s -> error $ "Unexpected status code on publish endpoint: " <> show s
  where
    body = A.toJSON request
    opts = defaults & header "X-Publish-Token" .~ [encodeUtf8 token]

-- | Request proof-of-work challenge for publish
-- Calls @\/api\/request-challenge@
--
-- >>> runDefaultAPI requestChallenge
-- Challenge {prefix = "BVNC...XoVu1H", target = "000000FF0...00000"}
requestChallenge :: API Challenge
requestChallenge = do
  url <- ask
  resp <- liftIO $ post (url <> "/request-challenge") $ A.toJSON ()
  pure $ decode (resp ^. responseBody)

-- | Publish Lyrics (request and solve challenge)
-- Calls @\/api\/publish@
publish :: PublishRequest -> API PublishResponse
publish r = do
  c <- requestChallenge
  publish' (solveChallenge c) r

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
