{-# LANGUAGE RecordWildCards #-}
module Text.TinySegmenter
  ( tokenize
  , tok
  )
where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import           Data.Char
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.Array               as A
import qualified Data.Text.Internal            as TI
import           Data.Word
import           Text.TinySegmenter.Score

import qualified Data.Text.IO                  as T

takeThree :: T.Text -> (Int, Int, Int, T.Text)
takeThree text = case T.uncons text of
  Nothing       -> (e1, e2, e3, text)
  Just (ca, ra) -> case T.uncons ra of
    Nothing       -> (ord ca, e1, e2, ra)
    Just (cb, rb) -> case T.uncons rb of
      Nothing       -> (ord ca, ord cb, e1, rb)
      Just (cc, rc) -> (ord ca, ord cb, ord cc, rc)
{-# INLINE takeThree #-}

isPair :: Int -> Bool
isPair c = c > 0x10000
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
    let idxList =
          let f x = if x < 0 then [] else x : f (pred x)
          in  zip xs $ f (pred size)
    arr <- A.new size
    forM_ idxList (\(c, i) -> A.unsafeWrite arr i c)
    A.unsafeFreeze arr
{-# INLINABLE tokenToText #-}

data TokenizeState = TS { remain :: {-# UNPACK #-} !T.Text
                        , results :: ![T.Text]
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
                     deriving (Show)

initialState :: T.Text -> TokenizeState
initialState text =
  let (a, b, c, rmn) = takeThree text
  in  TS { remain      = rmn
         , results     = []
         , token       = []
         , tokenLength = 0
         , score       = bias
         , p1          = u
         , p2          = u
         , p3          = u
         , w1          = b1
         , w2          = b2
         , w3          = b3
         , w4          = a
         , w5          = b
         , w6          = c
         , c1          = o
         , c2          = o
         , c3          = o
         , c4          = getCTypes a
         , c5          = getCTypes b
         , c6          = getCTypes c
         }
{-# INLINABLE initialState #-}

moveNext :: State TokenizeState ()
moveNext = do
  s@TS {..} <- get
  when (w4 < e1) $ do
    let (newW6, newRemain)
          | T.length remain > 0 = (ord $ T.head remain, T.tail remain)
          | w6 == e1            = (e2, T.empty)
          | w6 == e2            = (e3, T.empty)
          | otherwise           = (e1, T.empty)
    put $ s { remain = newRemain
            , score  = bias
            , w1     = w2
            , w2     = w3
            , w3     = w4
            , w4     = w5
            , w5     = w6
            , w6     = newW6
            , c1     = c2
            , c2     = c3
            , c3     = c4
            , c4     = c5
            , c5     = c6
            , c6     = getCTypes newW6
            }
{-# INLINABLE moveNext #-}

updateScore :: State TokenizeState ()
updateScore = do
  modify $ \s -> update s $ up1 (p1 s)
  modify $ \s -> update s $ up2 (p2 s)
  modify $ \s -> update s $ up3 (p3 s)
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
  update s i = s { score = score s + i }
  {-# INLINE update #-}
{-# INLINABLE updateScore #-}

pushToToken :: State TokenizeState ()
pushToToken = do
  s@TS {..} <- get
  when (w3 < e1) $ do
    let (newToken, newTokenLength)
          | isPair w3
          = let (upper, lower) = splitToWord16 w3
            in  (lower : upper : token, tokenLength + 2)
          | otherwise
          = (toEnum w3 : token, tokenLength + 1)
    put $ s { token = newToken, tokenLength = newTokenLength }
{-# INLINABLE pushToToken #-}

isFinished :: State TokenizeState Bool
isFinished = (e1 ==) <$> gets w4
{-# INLINABLE isFinished #-}

tailToResult :: State TokenizeState ()
tailToResult = do
  s@TS {..} <- get
  let word = tokenToText token tokenLength
  put $ s { results = word : results, token = [], tokenLength = 0 }
{-# INLINABLE tailToResult #-}

evalScore :: State TokenizeState (Maybe T.Text)
evalScore = do
  s@TS {..} <- get
  if score > 0
    then do
      let word = tokenToText token tokenLength
      put $ s { results     = word : results
              , token       = []
              , tokenLength = 0
              , p1          = p2
              , p2          = p3
              , p3          = b
              }
      return $ Just word
    else do
      put $ s { p1 = p2, p2 = p3, p3 = o }
      return Nothing
{-# INLINABLE evalScore #-}

tokenizeM :: State TokenizeState [T.Text]
tokenizeM = do
  moveNext
  pushToToken
  updateScore
  evalScore
  flag <- isFinished
  if not flag
    then tokenizeM
    else do
      tailToResult
      L.reverse <$> gets results
{-# INLINABLE tokenizeM #-}

tokenize :: T.Text -> [T.Text]
tokenize text = evalState tokenizeM $ initialState text
{-# INLINABLE tokenize #-}

tokM :: State TokenizeState (Maybe T.Text, Bool)
tokM = do
  moveNext
  pushToToken
  updateScore
  word <- evalScore
  flag <- isFinished
  return (word, flag)

tok :: T.Text -> [T.Text]
tok text =
  let f = runState tokM
      g x = case f x of
        ((Just t, _), s) -> Just (t, s)
        ((Nothing, isFinished), s) ->
          if isFinished then
            Nothing
          else
            g s
      s = initialState text
  in L.unfoldr g s
