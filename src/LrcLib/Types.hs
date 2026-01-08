{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LrcLib.Types
  ( API,
    Url,
    SearchResponse,
    Challenge (..),
    GetResponse (..),
    TrackData (..),
    SearchQuery (..),
    PublishResponse (..),
    PublishRequest (..),
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Representation of API url
type Url = String

-- | Monad for API actions that includes API url
type API = ReaderT Url IO

-- | Crypto proof-of-work challenge for publish
data Challenge
  = Challenge
  { challengePrefix :: Text,
    challengeTarget :: Text
  }
  deriving (Generic)

instance A.FromJSON Challenge where
  parseJSON = A.withObject "Challenge" $
    \v ->
      Challenge
        <$> v A..: "prefix"
        <*> v A..: "target"

-- | Track data that is accepted when getting lyrics
data TrackData
  = TrackData
  { trackId :: Integer,
    trackName :: Text,
    trackArtist :: Text,
    trackAlbum :: Text,
    trackDuration :: Integer,
    trackInstrumental :: Bool,
    trackPlainLyrics :: Maybe Text,
    trackSyncedLyrics :: Maybe Text
  }
  deriving (Generic)

instance A.FromJSON TrackData where
  parseJSON = A.withObject "TrackData" $
    \v ->
      TrackData
        <$> v A..: "id"
        <*> v A..: "trackName"
        <*> v A..: "artistName"
        <*> v A..: "albumName"
        <*> v A..: "duration"
        <*> v A..: "instrumental"
        <*> v A..:? "plainLyrics"
        <*> v A..:? "syncedLyrics"

-- | Response when getting lyrics
data GetResponse
  = NotFound
  | OK TrackData
  deriving (Generic, A.FromJSON, Show)

-- | Representation of request for publishing lyrics
data PublishRequest
  = PublishRequest
  { pubName :: Text,
    pubArtist :: Text,
    pubAlbum :: Text,
    pubDuration :: Integer,
    pubLyrics :: Maybe Text,
    pubSyncedLyrics :: Maybe Text
  }
  deriving (Generic)

-- | Response when publishing lyrics
data PublishResponse = PublishOK | IncorrectToken

-- | Query for search. Either text or track+artist+album
data SearchQuery
  = TextQuery Text
  | TrackQuery
      { queryName :: Text,
        queryArtist :: Maybe Text,
        queryAlbum :: Maybe Text
      }

-- | Response when searching lyrics
type SearchResponse = [TrackData]

instance Show TrackData where
  show TrackData {trackId, trackName, trackArtist, trackAlbum} =
    T.unpack $
      T.concat
        [ "Track, id: ",
          T.show trackId,
          ", name: ",
          trackName,
          ", artist: ",
          trackArtist,
          ", album: ",
          trackAlbum
        ]

instance A.ToJSON PublishRequest where
  toJSON PublishRequest {..} =
    A.object
      [ "trackName" A..= pubName,
        "artistName" A..= pubArtist,
        "albumName" A..= pubAlbum,
        "duration" A..= pubDuration,
        "plainLyrics" A..= pubLyrics,
        "syncedLyrics" A..= pubSyncedLyrics
      ]
