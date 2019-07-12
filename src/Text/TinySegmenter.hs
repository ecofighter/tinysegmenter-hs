{-# LANGUAGE RecordWildCards #-}
module Text.TinySegmenter(tokenize) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import           Data.Char
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Array               as A
import qualified Data.Text.Internal            as TI
import qualified Data.HashMap.Strict           as M
import           Data.Word
import           Text.Score

bias = -332

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
                        , token :: [Word16]
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
  let (a, b, c, rmn) = takeThree text
  in  TS { remain      = rmn
         , token       = []
         , tokenLength = 0
         , score       = bias
         , p1          = mk2i U
         , p2          = mk2i U
         , p3          = mk2i U
         , w1          = b1
         , w2          = b2
         , w3          = b3
         , w4          = a
         , w5          = b
         , w6          = c
         , c1          = mk2i O
         , c2          = mk2i O
         , c3          = mk2i O
         , c4          = getCTypes a
         , c5          = getCTypes b
         , c6          = getCTypes c
         }
{-# INLINABLE initialState #-}

moveNext :: State TokenizeState ()
moveNext = do
  TS {..} <- get
  let (newToken, newTokenLength)
        | w3 == b3  = ([], 0)
        | isPair w3 = let (upper, lower) = splitToWord16 w3
                      in  (lower : upper : token, tokenLength + 2)
        | otherwise = (toEnum w3 : token, tokenLength + 1)
  let (newW6, newRemain)
        | T.length remain > 0 = (ord $ T.head remain, T.tail remain)
        | w6 == e1            = (e1, T.empty)
        | otherwise           = (e2, T.empty)
  put $ TS { remain      = newRemain
           , token       = newToken
           , tokenLength = newTokenLength
           , score       = bias
           , p1          = p1
           , p2          = p2
           , p3          = p3
           , w1          = w2
           , w2          = w3
           , w3          = w4
           , w4          = w5
           , w5          = w6
           , w6          = newW6
           , c1          = c2
           , c2          = c3
           , c3          = c4
           , c4          = c5
           , c5          = c6
           , c6          = getCTypes newW6
           }
{-# INLINABLE moveNext #-}

updateScore :: State TokenizeState ()
updateScore = do
  modify $ \s -> update s $ bp1 (p1 s, p2 s)
  modify $ \s -> update s $ bp2 (p2 s, p3 s)
  modify $ \s -> update s $ uw1 (w1 s)
  modify $ \s -> update s $ uw2 (w2 s)
  modify $ \s -> update s $ uw3 (w3 s)
  modify $ \s -> update s $ uw4 (w4 s)
  modify $ \s -> update s $ uw5 (w5 s)
  modify $ \s -> update s $ uw6 (w6 s)
  modify $ \s -> update s $ bw1 (w2 s, w3 s)
  modify $ \s -> update s $ bw2 (w3 s, w4 s)
  modify $ \s -> update s $ bw3 (w4 s, w5 s)
  modify $ \s -> update s $ tw1 (w1 s, w2 s, w3 s)
  modify $ \s -> update s $ tw2 (w2 s, w3 s, w4 s)
  modify $ \s -> update s $ tw3 (w3 s, w4 s, w5 s)
  modify $ \s -> update s $ tw4 (w4 s, w5 s, w6 s)
  modify $ \s -> update s $ uc1 (c1 s)
  modify $ \s -> update s $ uc2 (c2 s)
  modify $ \s -> update s $ uc3 (c3 s)
  modify $ \s -> update s $ uc4 (c4 s)
  modify $ \s -> update s $ uc5 (c5 s)
  modify $ \s -> update s $ uc6 (c6 s)
  modify $ \s -> update s $ bc1 (c2 s, c3 s)
  modify $ \s -> update s $ bc2 (c3 s, c4 s)
  modify $ \s -> update s $ bc3 (c4 s, c5 s)
  modify $ \s -> update s $ tc1 (c1 s, c2 s, c3 s)
  modify $ \s -> update s $ tc2 (c2 s, c3 s, c4 s)
  modify $ \s -> update s $ tc3 (c3 s, c4 s, c5 s)
  modify $ \s -> update s $ tc4 (c4 s, c5 s, c6 s)
  modify $ \s -> update s $ uq1 (p1 s, c1 s)
  modify $ \s -> update s $ uq2 (p2 s, c2 s)
  modify $ \s -> update s $ uq3 (p3 s, c3 s)
  modify $ \s -> update s $ bq1 (p2 s, c2 s, c3 s)
  modify $ \s -> update s $ bq2 (p2 s, c3 s, c4 s)
  modify $ \s -> update s $ bq3 (p3 s, c2 s, c3 s)
  modify $ \s -> update s $ bq4 (p3 s, c3 s, c4 s)
  modify $ \s -> update s $ tq1 (p2 s, c1 s, c2 s, c3 s)
  modify $ \s -> update s $ tq2 (p2 s, c2 s, c3 s, c4 s)
  modify $ \s -> update s $ tq3 (p3 s, c1 s, c2 s, c3 s)
  modify $ \s -> update s $ tq4 (p3 s, c2 s, c3 s, c4 s)
  where
    update :: TokenizeState -> Int -> TokenizeState
    update s i = s { score = i }
    {-# INLINE update #-}
{-# INLINABLE updateScore #-}

evalScore :: State TokenizeState (Maybe T.Text)
evalScore = do
  s@TS {..} <- get
  if score > 0 then do
    let word = tokenToText token tokenLength
    put $ s { token = []
            , tokenLength = 0
            , p1 = p2
            , p2 = p3
            , p3 = b
            }
    return $ Just word
  else do
    put $ s { p1 = p2
            , p2 = p3
            , p3 = o
            }
    return Nothing
{-# INLINABLE evalScore #-}

tokenizeM :: State TokenizeState (Maybe T.Text)
tokenizeM = do
  moveNext
  updateScore
  evalScore
{-# INLINABLE tokenizeM #-}

tokenize :: T.Text -> [T.Text]
tokenize text =
  let func = runState tokenizeM
      func' s = case func s of
        (Just t , s') -> Just (t, s')
        (Nothing, s') -> if token s' /= []
          then Just (tokenToText (token s') (tokenLength s'), s' { token = [] })
          else Nothing
      s = initialState text
  in  L.unfoldr func' s
{-# INLINABLE tokenize #-}
