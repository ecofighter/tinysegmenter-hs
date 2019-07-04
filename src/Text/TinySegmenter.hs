{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.TinySegmenter where

import           Control.Monad.Trans.State
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
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
                        , seg :: Seq Char
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
takeThree text = case T.uncons text of
  Nothing -> (# (# | () #), (# | () #), (# | () #), text #)
  Just (ca, ra) -> case T.uncons ra of
    Nothing -> (# (# ca | #), (# | () #), (# | () #), ra #)
    Just (cb, rb) -> case T.uncons rb of
      Nothing -> (# (# ca | #), (# cb | #), (# | () #), rb #)
      Just (cc, rc) ->(# (# ca | #), (# cb | #), (# cc | #), rc #)
{-# INLINE takeThree #-}

mapCType :: (# Char | () #) -> Int#
mapCType (# a | #) = getCTypes a
mapCType (# | () #) = mk2i O
{-# INLINE mapCType #-}

makeInitialState :: T.Text -> TokenizeState
makeInitialState text =
  let (# a, b, c, rmn #) = takeThree text in
  TS { remain = rmn
     , score = bias
     , p1 = mk2i U
     , p2 = mk2i U
     , p3 = mk2i U
     , w1 = (# | () #)
     , w2 = (# | () #)
     , w3 = (# | () #)
     , w4 = a
     , w5 = b
     , w6 = c
     , c1 = mk2i O
     , c2 = mk2i O
     , c3 = mk2i O
     , c4 = mapCType a
     , c5 = mapCType b
     , c6 = mapCType c
     }
  where
    bias = -332#
{-# INLINE makeInitialState #-}
