{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.TinySegmenter where

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Identity
import           Data.Sequence
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import           Data.Text                     as T
import           GHC.Prim

data Marker = U | O | B
data CTypes = TM | TH | TI | TK | TA | TN | TO

mk2i m = case m of
  U -> 85#
  O -> 79#
  B -> 66#
{-# INLINE mk2i #-}

ct2i c = case c of
  TM -> 77#
  TH -> 72#
  TI -> 73#
  TK -> 75#
  TA -> 65#
  TN -> 78#
  TO -> 79#
{-# INLINE ct2i #-}

getCTypes :: Char -> Int#
getCTypes c
  | S.member c m
  = ct2i TM
  | ('一' <= c && c <= '龠') || S.member c h
  = ct2i TH
  | 'ぁ' <= c && c <= 'ん'
  = ct2i TI
  | ('ァ' <= c && c <= 'ヴ') || ('ｱ' <= c && c <= 'ﾝ') || S.member c ksub
  = ct2i TK
  | ('a' <= c && c <= 'z')
    || ('A' <= c && c <= 'Z')
    || ('ａ' <= c && c <= 'ｚ')
    || ('Ａ' <= c && c <= 'Ｚ')
  = ct2i TA
  | ('0' <= c && c <= '9') || ('０' <= c && c <= '９')
  = ct2i TN
  | otherwise
  = ct2i TO
 where
  m    = $([| S.fromList "一二三四五六七八九十百千万億兆" |])
  h    = $([| S.fromList "々〆ヵヶ" |])
  ksub = $([| S.fromList "ーｰ\xff9e" |])
{-# INLINABLE getCTypes #-}

data TokenizeState = TS { remain :: !T.Text
                        , score :: !Int#
                        , p1 :: !Int#
                        , p2 :: !Int#
                        , p3 :: !Int#
                        , w1 :: !(# Char | () #)
                        , w2 :: !(# Char | () #)
                        , w3 :: !(# Char | () #)
                        , w4 :: !(# Char | () #)
                        , w5 :: !(# Char | () #)
                        , w6 :: !(# Char | () #)
                        , c1 :: !Int#
                        , c2 :: !Int#
                        , c3 :: !Int#
                        , c4 :: !Int#
                        , c5 :: !Int#
                        , c6 :: !Int#
                        }

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f
{-# INLINE nTimes #-}

takeThree :: T.Text -> (# (# Char | () #), (# Char | () #), (# Char | () #), T.Text #)
takeThree text
  | l >= 3
  = (# (# a | #), (# b | #), (# c | #), nTimes 3 T.tail text #)
  | l == 2
  = (# (# a | #), (# b | #), (# | () #), nTimes 2 T.tail text #)
  | l == 1
  = (# (# a | #), (# | () #), (# | () #), nTimes 1 T.tail text #)
  | otherwise
  = (# (# | () #), (# | () #), (# | () #), nTimes 0 T.tail text #)
  where
    !l = T.length text
    a = T.head text
    b = T.head $ T.tail text
    c = T.head . T.tail $ T.tail text
{-# INLINE takeThree #-}

mapCType3 :: (# Char, Char, Char #) -> (# Int#, Int#, Int# #)
mapCType3 (# a, b, c #) = (# getCTypes a, getCTypes b, getCTypes c #)
{-# INLINE mapCType3 #-}

mapCType2 :: (# Char, Char #) -> (# Int#, Int# #)
mapCType2 (# a, b #) = (# getCTypes a, getCTypes b #)
{-# INLINE mapCType2 #-}

makeInitialState :: T.Text -> TokenizeState
makeInitialState text = do
  let (# a, b, c, rmn #) = takeThree text
  TS { remain = rmn
     , score = bias
     , p1 = mk2i U
     , p2 = mk2i U
     , p3 = mk2i U
     }
  where
    bias = -332#
{-# INLINE makeInitialState #-}
