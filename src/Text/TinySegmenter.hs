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

-- Words
h = ct2i TH
i = ct2i TI
k = ct2i TK
o = ct2i TO
a = ct2i TA
n = ct2i TN
m = ct2i TM

b = mk2i B
u = mk2i U

bc1 t | t == (h, h) = 6
      | t == (i, i) = 2461
      | t == (k, h) = 406
      | t == (o, h) = -1378
{-# INLINE bc1 #-}

bc2 t | t == (a, a) = -3267
      | t == (a, i) = 2744
      | t == (a, n) = -878
      | t == (h, h) = -4070
      | t == (h, m) = -1711
      | t == (h, n) = 4012
      | t == (h, o) = 3761
      | t == (i, a) = 1327
      | t == (i, h) = -1184
      | t == (i, i) = -1332
      | t == (i, k) = 1721
      | t == (i, o) = 5492
      | t == (k, i) = 3831
      | t == (k, k) = -8741
      | t == (m, h) = -3132
      | t == (m, k) = 3334
      | t == (o, o) = -2920
{-# INLINE bc2 #-}

bc3 t | t == (h, h) = 996
      | t == (h, i) = 626
      | t == (h, k) = -721
      | t == (h, n) = -1307
      | t == (h, o) = -836
      | t == (i, h) = -301
      | t == (k, k) = 2762
      | t == (m, k) = 1079
      | t == (m, m) = 4034
      | t == (o, a) = -1652
      | t == (o, h) = 266
{-# INLINE bc3 #-}

bp1 t | t == (b, b) = 295
      | t == (o, b) = 304
      | t == (o, o) = -125
      | t == (u, b) = 352
{-# INLINE bp1 #-}

bp2 t | t == (b, o) = 60
      | t == (o, o) = -1762
{-# INLINE bp2 #-}

bq1 t | t == (b, h, h) = 1150
      | t == (b, h, m) = 1521
      | t == (b, i, i) = -1158
      | t == (b, i, m) = 886
      | t == (b, m, h) = 1208
      | t == (b, n, h) = 449
      | t == (b, o, h) = -91
      | t == (b, o, o) = -2597
      | t == (o, h, i) = 451
      | t == (o, i, h) = -296
      | t == (o, k, a) = 1851
      | t == (o, k, h) = -1020
      | t == (o, k, k) = 904
      | t == (o, o, o) = 2965
{-# INLINE bq1 #-}

bq2 t | t == (b, h, h) = 118
      | t == (b, h, i) = -1159
      | t == (b, h, m) = 466
      | t == (b, i, h) = -919
      | t == (b, k, k) = -1720
      | t == (b, k, o) = 864
      | t == (o, h, h) = -1139
      | t == (o, h, m) = -181
      | t == (o, i, h) = 153
      | t == (u, h, i) = -1146
{-# INLINE bq2 #-}

bq3 t | t == (b, h, h) = -792
      | t == (b, h, i) = 2664
      | t == (b, i, i) = -299
      | t == (b, k, i) = 419
      | t == (b, m, h) = 937
      | t == (b, m, m) = 8335
      | t == (b, n, n) = 998
      | t == (b, o, h) = 775
      | t == (o, h, h) = 2174
      | t == (o, h, m) = 439
      | t == (o, i, i) = 280
      | t == (o, k, h) = 1798
      | t == (o, k, i) = -793
      | t == (o, k, o) = -2242
      | t == (o, m, h) = -2402
      | t == (o, o, o) = 11699
{-# INLINE bq3 #-}

bq4 t | t == (b, h, h) = -3895
      | t == (b, i, i) = 3761
      | t == (b, i, i) = -4654
      | t == (b, i, i) = 1348
      | t == (b, k, k) = -1806
      | t == (b, m, m) = -3385
      | t == (b, o, o) = -12396
      | t == (o, a, a) = 926
      | t == (o, h, h) = 266
      | t == (o, h, h) = -2036
      | t == (o, n, n) = -973
{-# INLINE bq4 #-}
