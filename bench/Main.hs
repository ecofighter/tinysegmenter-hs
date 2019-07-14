module Main where

import           Criterion
import           Criterion.Main
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.TinySegmenter

setupEnv :: IO T.Text
setupEnv = T.readFile "timemachineu8j.txt"

main :: IO ()
main = defaultMain [
     env setupEnv $ \text -> bgroup "tokenize" [
         bench "lazy list" $ nf tokenize text
         , bench "lazy list(only head)" $ nf (head . tokenize) text
         , bench "strict list" $ whnf tokenize' text
         , bench "strict list(only head)" $ whnf (head . tokenize') text
         , bench "vector" $ nf tokenizeToVec text
     ]
  ]
