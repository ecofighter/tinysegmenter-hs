{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.TinySegmenter where

import           Control.Monad.Trans.State
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import qualified Data.Text                     as T
import qualified Data.Text.Array               as TA
import           GHC.Exts

data Marker = U | O | B
data CTypes = TM | TH | TI | TK | TA | TN | TO

mk2i m = case m of
  U -> fromEnum 'U'
  O -> fromEnum 'O'
  B -> fromEnum 'B'
{-# INLINE mk2i #-}

ct2i c = case c of
  TM -> fromEnum 'M'
  TH -> fromEnum 'H'
  TI -> fromEnum 'I'
  TK -> fromEnum 'K'
  TA -> fromEnum 'A'
  TN -> fromEnum 'N'
  TO -> fromEnum 'O'
{-# INLINE ct2i #-}

getCTypes :: Char -> Int
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

takeThree :: T.Text -> (# (# Char | () #), (# Char | () #), (# Char | () #), T.Text #)
takeThree text = case T.uncons text of
  Nothing -> (# (# | () #), (# | () #), (# | () #), text #)
  Just (ca, ra) -> case T.uncons ra of
    Nothing -> (# (# ca | #), (# | () #), (# | () #), ra #)
    Just (cb, rb) -> case T.uncons rb of
      Nothing -> (# (# ca | #), (# cb | #), (# | () #), rb #)
      Just (cc, rc) ->(# (# ca | #), (# cb | #), (# cc | #), rc #)
{-# INLINE takeThree #-}

mapCType :: (# Char | () #) -> Int
mapCType (# a | #) = getCTypes a
mapCType (# | () #) = mk2i O
{-# INLINE mapCType #-}

data TokenizeState = TS { remain :: !T.Text
                        -- , word :: LT.Builder
                        , score :: {-# UNPACK #-} !Int
                        , p1 :: {-# UNPACK #-} !Int
                        , p2 :: {-# UNPACK #-} !Int
                        , p3 :: {-# UNPACK #-} !Int
                        , w1 :: !(# Char | () #)
                        , w2 :: !(# Char | () #)
                        , w3 :: !(# Char | () #)
                        , w4 :: !(# Char | () #)
                        , w5 :: !(# Char | () #)
                        , w6 :: !(# Char | () #)
                        , c1 :: {-# UNPACK #-} !Int
                        , c2 :: {-# UNPACK #-} !Int
                        , c3 :: {-# UNPACK #-} !Int
                        , c4 :: {-# UNPACK #-} !Int
                        , c5 :: {-# UNPACK #-} !Int
                        , c6 :: {-# UNPACK #-} !Int
                        }

makeInitialState :: T.Text -> TokenizeState
makeInitialState text =
  let !(# a, b, c, rmn #) = takeThree text in
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
    bias = -332
{-# INLINE makeInitialState #-}
