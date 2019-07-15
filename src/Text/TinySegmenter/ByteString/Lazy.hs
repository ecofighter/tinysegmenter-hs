{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Text.TinySegmenter.ByteString.Lazy
  ( tokenize
  , tokenizeToVec
  )
where

import           Control.Exception
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Bits
import           Data.Functor.Identity
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.List                     as L
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV
import           Data.Typeable
import           Data.Word
import           Data.Int

-- import           Text.TinySegmenter.Model
#define INCLUDE_MODEL
#include "../Model.hs"

data InvalidUtf8ByteException = InvalidUtf8ByteException
  deriving (Show, Typeable)

instance Exception InvalidUtf8ByteException where

getFollowsBytesNum :: Word8 -> Int64
getFollowsBytesNum w | w < toEnum 0x80 = 1
                     | w < toEnum 0xe0 = 2
                     | w < toEnum 0xf0 = 3
                     | w < toEnum 0xf8 = 4
                     | otherwise       = -1
{-# INLINE getFollowsBytesNum #-}

getThreeCharsLen :: BSL.ByteString -> (Int64, Int64, Int64)
getThreeCharsLen bs =
  let len = BSL.length bs in
    if | len == 0 -> (-1,-1,-1)
       | otherwise ->
         let fs = getFollowsBytesNum (BSL.head bs) in
           if | len <= fs -> (fs, -1, -1)
              | otherwise ->
                let sn = getFollowsBytesNum $ BSL.index bs fs in
                  if | len <= fs+sn -> (fs, sn, -1)
                     | otherwise -> (fs, sn, getFollowsBytesNum $ BSL.index bs (fs+sn))
{-# INLINE getThreeCharsLen #-}

-- Data.Char.ord for ByteString
bsord :: BSL.ByteString -> Int
bsord bs =
  let c = BSL.head bs
  in
    if
      | c < toEnum 0x80
        -> toEnum $ fromEnum c
      | c < toEnum 0xe0
        -> let u = fromEnum $ c .&. 0x1F
               l = fromEnum $ BSL.index bs 1 .&. 0x3F
           in  unsafeShiftL u 6 .&. l
      | c < toEnum 0xf0
        -> let u = fromEnum $ c .&. 0x0F
               m = fromEnum $ BSL.index bs 1 .&. 0x3F
               l = fromEnum $ BSL.index bs 2
                     .&. 0x3F
           in unsafeShiftL u 12 .&. unsafeShiftL m 6 .&. l
      | c < toEnum 0xf8
        -> let
             fi = fromEnum $ c .&. 0x07
             sn = fromEnum $ BSL.index bs 1 .&. 0x3F
             th = fromEnum $ BSL.index bs 2 .&. 0x3F
             fo = fromEnum $ BSL.index bs 3 .&. 0x3F
           in
             unsafeShiftL fi 18
             .&. unsafeShiftL sn 12
             .&. unsafeShiftL th 6
             .&. fo
      | otherwise
        -> throw InvalidUtf8ByteException
{-# INLINE bsord #-}

data TokenizeState = TS { remain :: !BSL.ByteString
                        , tokenLength :: {-# UNPACK #-} !Int64 -- also index of byte now looking, w3's first byte
                        , score :: {-# UNPACK #-} !Int
                        , p1 :: !Marker
                        , p2 :: !Marker
                        , p3 :: !Marker
                        , w1 :: {-# UNPACK #-} !Int
                        , w2 :: {-# UNPACK #-} !Int
                        , w3 :: {-# UNPACK #-} !Int
                        , w4 :: {-# UNPACK #-} !Int
                        , w5 :: {-# UNPACK #-} !Int
                        , w6 :: {-# UNPACK #-} !Int
                        , w4len :: {-# UNPACK #-} !Int64
                        , w5len :: {-# UNPACK #-} !Int64
                        , w6len :: {-# UNPACK #-} !Int64
                        , c1 :: !CType
                        , c2 :: !CType
                        , c3 :: !CType
                        , c4 :: !CType
                        , c5 :: !CType
                        , c6 :: !CType
                        }

initialState :: BSL.ByteString -> TokenizeState
initialState text =
  let (lena, lenb, lenc) = getThreeCharsLen text
      one | lena > 0     = bsord $ BSL.take lena text
          | otherwise    = e1
      two | lenb > 0     = bsord . BSL.take lenb $
                             BSL.drop lena text
          | one == e1    = e2
          | otherwise    = e1
      three | lenc > 0   = bsord . BSL.take lenc $
                             BSL.drop (lena+lenc) text
            | two == e2  = e3
            | two == e1  = e2
            | otherwise  = e1
  in  TS { remain      = text
         , tokenLength = 0
         , score       = bias
         , p1          = MU
         , p2          = MU
         , p3          = MU
         , w1          = b1
         , w2          = b2
         , w3          = b3
         , w4          = one
         , w5          = two
         , w6          = three
         , w4len       = lena
         , w5len       = lenb
         , w6len       = lenc
         , c1          = CTO
         , c2          = CTO
         , c3          = CTO
         , c4          = getCTypes one
         , c5          = getCTypes two
         , c6          = getCTypes three
         }
{-# INLINE initialState #-}

moveNext :: Monad m => StateT TokenizeState m ()
moveNext = do
  s@TS {..} <- get
  let offset = tokenLength + w4len + w5len + w6len
  let newW6Len | BSL.length remain > offset
                 = getFollowsBytesNum $ BSL.index remain offset
               | otherwise = -1
  let newW6 | newW6Len > 0 = bsord $ BSL.take newW6Len $ BSL.drop offset remain
            | w6 >= e2     = e3
            | w6 == e1     = e2
            | otherwise    = e1
  put $ s { score       = bias
          , tokenLength = tokenLength + w4len
          , w1          = w2
          , w2          = w3
          , w3          = w4
          , w4          = w5
          , w5          = w6
          , w6          = newW6
          , w4len       = w5len
          , w5len       = w6len
          , w6len       = newW6Len
          , c1          = c2
          , c2          = c3
          , c3          = c4
          , c4          = c5
          , c5          = c6
          , c6          = getCTypes newW6
          }
{-# INLINE moveNext #-}

updateScore :: Monad m => StateT TokenizeState m ()
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
  update s num = s { score = score s + num }
  {-# INLINE update #-}
{-# INLINE updateScore #-}

isFinished :: Monad m => StateT TokenizeState m Bool
isFinished = do
  remlen <- BSL.length <$> gets remain
  toklen <- gets tokenLength
  return $ toklen == remlen
{-# INLINE isFinished #-}

tailToResult :: Monad m => StateT TokenizeState m (Maybe BSL.ByteString)
tailToResult = do
  s@TS {..} <- get
  let remlen = BSL.length remain
  if remlen > 0
    then do
      put $ s { remain = BSL.empty, tokenLength = 0 }
      return $ Just remain
    else return Nothing
{-# INLINE tailToResult #-}

evalScore :: Monad m => StateT TokenizeState m (Maybe BSL.ByteString)
evalScore = do
  s@TS {..} <- get
  if score > 0
    then do
      let (word, newRem) = BSL.splitAt tokenLength remain
      put $ s { remain = newRem
              , tokenLength = 0
              , p1    = p2
              , p2    = p3
              , p3    = MB
              }
      return $ Just word
    else do
      put $ s { p1 = p2, p2 = p3, p3 = MO }
      return Nothing
{-# INLINE evalScore #-}

evalOneStep :: Monad m => StateT TokenizeState m (Maybe BSL.ByteString)
evalOneStep = do
  moveNext
  updateScore
  evalScore
{-# INLINE evalOneStep #-}

tokenizeM :: StateT TokenizeState Identity (Maybe BSL.ByteString, Bool)
tokenizeM = do
  flag <- isFinished
  if flag
    then do
      word <- tailToResult
      return (word, True)
    else do
      word <- evalOneStep
      return (word, False)
{-# INLINE tokenizeM #-}

tokenize :: BSL.ByteString -> [BSL.ByteString]
tokenize text =
  let f = runState tokenizeM
      g x = case f x of
        ((Just t , _   ), s) -> Just (t, s)
        ((Nothing, flag), s) -> if flag then Nothing else g s
  in  L.unfoldr g $ initialState text
{-# INLINE tokenize #-}

tokenizeToVecM :: StateT TokenizeState (ST s) (MV.MVector s BSL.ByteString, Int)
tokenizeToVecM = do
  len <- BSL.length <$> gets remain
  vec <- lift $ MV.unsafeNew $ fromIntegral len
  body (vec, 0)
 where
  body (!vec, !idx) = do
    flag <- isFinished
    if flag
      then do
        word <- tailToResult
        case word of
          Just w -> do
            lift $ MV.unsafeWrite vec idx w
            return (vec, idx + 1)
          Nothing -> return (vec, idx)
      else do
        word <- evalOneStep
        case word of
          Just w -> do
            lift $ MV.unsafeWrite vec idx w
            body (vec, idx + 1)
          Nothing -> body (vec, idx)
{-# INLINE tokenizeToVecM #-}

tokenizeToVec :: BSL.ByteString -> V.Vector BSL.ByteString
tokenizeToVec text = v
 where
  v = runST $ do
    (vec, len) <- evalStateT tokenizeToVecM $ initialState text
    V.freeze $ MV.unsafeSlice 0 len vec
{-# INLINE tokenizeToVec #-}
