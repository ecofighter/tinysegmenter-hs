{-# LANGUAGE OverloadedStrings #-}
module Main where

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

import Data.String
import Control.Exception

before :: IsString a => a
before = "私の名前は中野です"
expect :: IsString a => [a]
expect = ["私", "の", "名前", "は", "中野", "です"]

main :: IO ()
main = do
  assert (TT.tokenize before == expect) $ return ()
  assert (TTL.tokenize before == expect) $ return ()
  assert (TBS.tokenize before == expect) $ return ()
  assert (TBSL.tokenize before == expect) $ return ()
  return ()

