{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy as BS
import Data.Kind
import Data.Maybe
import LrcLib.Client
import Test.Tasty
import Test.Tasty.HUnit

decodeTest :: forall (a :: Type). (FromJSON a) => String -> TestTree
decodeTest file = testCase file $ void (fromJust . decode @a <$> BS.readFile file)

main :: IO ()
main =
  defaultMain . testGroup "decoding" $
    [ decodeTest @Challenge "test/challenge.json",
      decodeTest @GetResponse "test/getUrlSuccess.json",
      decodeTest @GetResponse "test/getUrlNotFound.json",
      decodeTest @SearchResponse "test/search.json"
    ]
