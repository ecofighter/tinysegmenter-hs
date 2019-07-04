{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
module Text.TinySegmenter where

import           Control.DeepSeq
import           Control.Monad.Trans.State
import qualified Data.HashSet                  as S
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
  | '一' <= c && c <= '龠'
    || S.member c h
  = ct2i TH
  | 'ぁ' <= c && c <= 'ん'
  = ct2i TI
  | 'ァ' <= c && c <= 'ヴ'
    || 'ｱ' <= c && c <= 'ﾝ'
    || S.member c ksub
  = ct2i TK
  | 'a' <= c && c <= 'z'
    || 'A' <= c && c <= 'Z'
    || 'ａ' <= c && c <= 'ｚ'
    || 'Ａ' <= c && c <= 'Ｚ'
  = ct2i TA
  | '0' <= c && c <= '9'
    || '０' <= c && c <= '９'
  = ct2i TN
 where
  !m    = let m = S.fromList "一二三四五六七八九十百千万億兆" in deepseq m m
  !h    = let h = S.fromList "々〆ヵヶ" in deepseq h h
  !ksub = let ksub = S.fromList "ーｰ\xff9e" in deepseq ksub ksub
