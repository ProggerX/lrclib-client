module Main where

import Data.Aeson
import Data.ByteString.Lazy as BS
import Data.Maybe
import LrcLib.Client
import Test.Tasty
import Test.Tasty.HUnit

decodeTest file = testCase file $ (fromJust . decode <$> BS.readFile file)

main :: IO ()
main =
  defaultMain . testGroup "decoding" . fmap decodeTest $
    [ "test/challenge.json",
      "test/getUrlSuccess.json",
      "test/getUrlNotFound.json",
      "test/search.json"
    ]
