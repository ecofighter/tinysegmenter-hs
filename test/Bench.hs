module Main where

import           Criterion
import           Criterion.Main
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Text.TinySegmenter.Text       as TT
import           Text.TinySegmenter.Text.Lazy  as TTL
import           Text.TinySegmenter.ByteString as TBS
import           Text.TinySegmenter.ByteString.Lazy
                                               as TBSL

setupText :: IO T.Text
setupText = T.readFile "timemachineu8j.txt"

setupLText :: IO TL.Text
setupLText = TL.readFile "timemachineu8j.txt"

setupBS :: IO BS.ByteString
setupBS = BS.readFile "timemachineu8j.txt"

setupLBS :: IO BSL.ByteString
setupLBS = BSL.readFile "timemachineu8j.txt"

main :: IO ()
main = defaultMain
  [ bgroup
      "tinysegmenter"
      [ env setupText $ \text -> bgroup
          "text"
          [ bench "list" $ nf TT.tokenize text
          , bench "list(only head)" $ nf (head . TT.tokenize) text
          , bench "vector" $ nf TT.tokenizeToVec text
          ]
      , env setupLText $ \text -> bgroup
          "lazy text"
          [ bench "list" $ nf TTL.tokenize text
          , bench "list(only head)" $ nf (head . TTL.tokenize) text
          , bench "vector" $ nf TTL.tokenizeToVec text
          ]
      , env setupBS $ \text -> bgroup
          "bytestring"
          [ bench "list" $ nf TBS.tokenize text
          , bench "list(only head)" $ nf (head . TBS.tokenize) text
          , bench "vector" $ nf TBS.tokenizeToVec text
          ]
      , env setupLBS $ \text -> bgroup
          "lazy bytestring"
          [ bench "list" $ nf TBSL.tokenize text
          , bench "list(only head)" $ nf (head . TBSL.tokenize) text
          , bench "vector" $ nf TBSL.tokenizeToVec text
          ]
      ]
  ]
