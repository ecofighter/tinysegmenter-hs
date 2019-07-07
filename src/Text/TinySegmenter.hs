{-# LANGUAGE BangPatterns #-}
module Text.TinySegmenter where

import           Control.Monad.ST
import           Data.Bits
import           Data.Char
import qualified Data.Text                     as T
import qualified Data.Text.Array               as A
import qualified Data.Text.Internal            as TI
import qualified Data.HashMap.Strict as M
import           Data.Word

-- Markers, whose values are out of unicode code point range
b1, b2, b3, e1, e2, e3 :: Int
b1 = 0x110001
b2 = 0x110002
b3 = 0x110003
e1 = 0x110004
e2 = 0x110005
e3 = 0x110006

data Marker = U | O | B
data CTypes = TM | TH | TI | TK | TA | TN | TO

mk2i :: Marker -> Word8
mk2i m = case m of
  U -> fromIntegral $ ord 'U'
  O -> fromIntegral $ ord 'O'
  B -> fromIntegral $ ord 'B'
{-# INLINE mk2i #-}

ct2i :: CTypes -> Word8
ct2i c = case c of
  TM -> fromIntegral $ ord 'M'
  TH -> fromIntegral $ ord 'H'
  TI -> fromIntegral $ ord 'I'
  TK -> fromIntegral $ ord 'K'
  TA -> fromIntegral $ ord 'A'
  TN -> fromIntegral $ ord 'N'
  TO -> fromIntegral $ ord 'O'
{-# INLINE ct2i #-}

getCTypes :: Int -> Word8
getCTypes c
  | c `elem` m
  = ct2i TM
  | (ord '一' <= c && c <= ord '龠') || c `elem` h
  = ct2i TH
  | ord 'ぁ' <= c && c <= ord 'ん'
  = ct2i TI
  | (ord 'ァ' <= c && c <= ord 'ヴ')
    || (ord 'ｱ' <= c && c <= ord 'ﾝ') || c `elem` ksub
  = ct2i TK
  | (ord 'a' <= c && c <= ord 'z')
    || (ord 'A' <= c && c <= ord 'Z')
    || (ord 'ａ' <= c && c <= ord 'ｚ')
    || (ord 'Ａ' <= c && c <= ord 'Ｚ')
  = ct2i TA
  | (ord '0' <= c && c <= ord '9') || (ord '０' <= c && c <= ord '９')
  = ct2i TN
  | otherwise
  = ct2i TO
  where
    m    = fmap ord "一二三四五六七八九十百千万億兆"
    h    = fmap ord "々〆ヵヶ"
    ksub = fmap ord "ーｰ\xff9e"
{-# INLINABLE getCTypes #-}

takeThree :: T.Text -> (Int, Int, Int, T.Text)
takeThree text = case T.uncons text of
  Nothing -> (e1, e2, e3, text)
  Just (ca, ra) -> case T.uncons ra of
    Nothing -> (ord ca, e1, e2, ra)
    Just (cb, rb) -> case T.uncons rb of
      Nothing -> (ord ca, ord cb, e1, rb)
      Just (cc, rc) ->(ord ca, ord cb, ord cc, rc)
{-# INLINE takeThree #-}

takeOne :: T.Text -> Maybe (Int, T.Text)
takeOne = fmap (\(c, r) -> (ord c, r)) . T.uncons
{-# INLINE takeOne #-}

isPair :: Int -> Bool
isPair = (< 0x10000)
{-# INLINE isPair #-}

type Upper = Word16
type Lower = Word16
splitToWord16 :: Int -> (Upper, Lower)
splitToWord16 c = (upper, lower)
  where
    m     = c - 0x10000
    upper = fromIntegral (shiftR m 10 + 0xD800)
    lower = fromIntegral ((m .&. 0x3FF) + 0xDC00)
{-# INLINE splitToWord16 #-}

tokenToText :: [Word16] -> Int -> T.Text
tokenToText xs size = TI.text array 0 size
  where
    array = runST $ do
      arr <- A.new size
      mapM_ (\(c, i) -> A.unsafeWrite arr i c) $ zip xs [(size-1) .. 0]
      A.unsafeFreeze arr
{-# INLINABLE tokenToText #-}

data TokenizeState = TS { remain :: {-# UNPACK #-} !T.Text
                        , token :: ![Word16]
                        , tokenLength :: {-# UNPACK #-} !Int
                        , score :: {-# UNPACK #-} !Int
                        , p1 :: {-# UNPACK #-} !Word8
                        , p2 :: {-# UNPACK #-} !Word8
                        , p3 :: {-# UNPACK #-} !Word8
                        , w1 :: {-# UNPACK #-} !Int
                        , w2 :: {-# UNPACK #-} !Int
                        , w3 :: {-# UNPACK #-} !Int
                        , w4 :: {-# UNPACK #-} !Int
                        , w5 :: {-# UNPACK #-} !Int
                        , w6 :: {-# UNPACK #-} !Int
                        , c1 :: {-# UNPACK #-} !Word8
                        , c2 :: {-# UNPACK #-} !Word8
                        , c3 :: {-# UNPACK #-} !Word8
                        , c4 :: {-# UNPACK #-} !Word8
                        , c5 :: {-# UNPACK #-} !Word8
                        , c6 :: {-# UNPACK #-} !Word8
                        }

initialState :: T.Text -> TokenizeState
initialState text =
  let !(a, b, c, rmn) = takeThree text in
  TS { remain = rmn
     , token = []
     , tokenLength = 0
     , score = bias
     , p1 = mk2i U
     , p2 = mk2i U
     , p3 = mk2i U
     , w1 = b1
     , w2 = b2
     , w3 = b3
     , w4 = a
     , w5 = b
     , w6 = c
     , c1 = mk2i O
     , c2 = mk2i O
     , c3 = mk2i O
     , c4 = getCTypes a
     , c5 = getCTypes b
     , c6 = getCTypes c
     }
  where
    bias = -332
{-# INLINABLE initialState #-}

