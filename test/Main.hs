{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Kind
import Data.Maybe
import LrcLib.Client
import Test.Tasty
import Test.Tasty.HUnit

decodeTest :: forall (a :: Type). (FromJSON a, Eq a, Read a, Show a) => FilePath -> TestTree
decodeTest file = testCase file $ do
  decoded <- fromJust . decode @a <$> BS.readFile (file <> ".json")
  expected <- read @a <$> readFile file
  decoded @?= expected

main :: IO ()
main =
  defaultMain . testGroup "" $
    [ testGroup
        "decoding"
        [ decodeTest @Challenge "test/challenge",
          decodeTest @TrackData "test/getUrlSuccess",
          decodeTest @SearchResponse "test/search"
        ],
      testCase
        "challenge solving"
        $ do
          let challenge = Challenge {prefix = "PREFIX", target = "000000FF00000000000000000000000000000000000000000000000000000000"}
          solveChallenge challenge @?= "PREFIX:1944868"
    ]
