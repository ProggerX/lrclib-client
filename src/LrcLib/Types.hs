{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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
  { prefix :: Text,
    target :: Text
  }
  deriving (A.FromJSON, Generic, Eq, Show, Read)

-- | Track data that is accepted when getting lyrics
data TrackData
  = TrackData
  { id :: Integer,
    trackName :: Text,
    artistName :: Text,
    albumName :: Text,
    duration :: Integer,
    instrumental :: Bool,
    plainLyrics :: Maybe Text,
    syncedLyrics :: Maybe Text
  }
  deriving (A.FromJSON, Generic, Eq, Read)

-- | Response when getting lyrics
data GetResponse
  = NotFound
  | OK TrackData
  deriving (Generic, A.FromJSON, Show)

-- | Representation of request for publishing lyrics
data PublishRequest
  = PublishRequest
  { trackName :: Text,
    artistName :: Text,
    albumName :: Text,
    duration :: Integer,
    plainLyrics :: Text,
    syncedLyrics :: Text
  }
  deriving (Generic, A.ToJSON)

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
  show track =
    T.unpack $
      T.concat
        [ "Track, id: ",
          T.show track.id,
          ", name: ",
          track.trackName,
          ", artist: ",
          track.artistName,
          ", album: ",
          track.albumName
        ]
