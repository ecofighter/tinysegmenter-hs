{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.TinySegmenter
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  putStrLn "Start"
  args <- getArgs
  file <- if not (null args) then T.readFile (head args) else T.getContents
  let strict = tokenize file
  let lazy = tok file
  mapM_ T.putStrLn lazy
  putStrLn "End"
  exitSuccess
