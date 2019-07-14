module Main where

import           Criterion
import           Criterion.Main
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.TinySegmenter.Text

setupEnv :: IO T.Text
setupEnv = T.readFile "timemachineu8j.txt"

main :: IO ()
main = defaultMain [
     env setupEnv $ \text -> bgroup "text" [
         bench "list" $ nf tokenize text
         , bench "list(only head)" $ nf (head . tokenize) text
         , bench "strict list" $ whnf tokenize' text
         , bench "strict list(only head)" $ whnf (head . tokenize') text
         , bench "vector" $ nf tokenizeToVec text
     ]
  ]
